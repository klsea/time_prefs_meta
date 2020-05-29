# Convert to effect sizes
# 5.13.20 KLS updated 5.29.19

# load required packages
library(here)
library(tidyverse)
library(metafor)

# load source functions

# set hard-coded variables
file <- 'cleaned.csv'

# load data
dt <- read.csv(here::here('data', file))

# make interaction term for study + conditions
dt$conditionID <- interaction(dt$Study.Identifier, dt$condition)

# Calculate effect sizes for extreme group designs
dm <- dt[which(dt$Design == 'extreme group'),] # pull out means
ds <- dm[is.na(dm$sd),]
dm <- dm[!is.na(dm$sd),]

### What to do about Garza 2016???
# Remove for now
dm <- dm[-c(grep('Garza', dm$Study.Identifier)),]
dm <- dm[c(1,2,6,8:10, 11:12, 14, 18:19, 23)]

dm <- pivot_wider(dm, 
                   id_cols = colnames(dm[c(1:6, 12)]), 
                   names_from = 'Intervention', 
                   values_from = c('mean', 'sd', 'n', 'age_mean', 'age_range'))

dm <- escalc(measure = 'SMD', n1i = n_Older, n2i = n_Younger, m1i = mean_Older, m2i = mean_Younger, sd1i = sd_Older, sd2i = sd_Younger, data=dm)


## average across multiple values within the same study
li <- dm[which(dm$Study.Identifier == 'Li 2013'),]
li <- cbind(li[1,1:16], t(colMeans(li[17:18])))
liu <- dm[which(dm$Study.Identifier == 'Liu 2016'),]
liu <- cbind(liu[1,1:16], t(colMeans(liu[17:18])))

## remove studies with multiple condition rows and replace with study means
dm <- dm[-which(dm$Study.Identifier == 'Li 2013'),]
dm <- rbind(dm, li)
dm <- dm[-which(dm$Study.Identifier == 'Liu 2016'),]
dm <- rbind(dm, liu)
rm(li, liu)

dm$mean_Older <- NULL
dm$mean_Younger <- NULL
dm$sd_Older <- NULL
dm$sd_Younger <- NULL
dm$conditionID <- NULL

## effect per decade
dm$age_diff = dm$age_mean_Older - dm$age_mean_Younger
dm$adj_effect_size <- (dm$yi/dm$age_diff) * 10 # calculate effect per year and then multiply by 10 for decade
dm$adj_variance <- (dm$vi/dm$age_diff) * 10 

write.csv(dm, here::here('output', 'extreme_group_table.csv'), row.names = FALSE)
