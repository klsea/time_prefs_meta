# Convert to effect sizes using esc package
# 6.2.2020 KLS

# load required packages
library(here)
library(tidyverse)
library(esc)

# load source functions
source(here::here('scr', 'reverse_es.R'))

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
dm <- dm[c(1,2,6,8:10, 11:14, 18:19, 23)]

dm <- pivot_wider(dm, 
                  id_cols = colnames(dm[c(1:6, 13)]), 
                  names_from = 'Intervention', 
                  values_from = c('mean', 'sd', 'n', 'age_mean', 'age_range', 'age_sd'))
dm <- mutate(dm, 
       effect_size = esc_mean_sd(grp1m = mean_Older, grp1sd = sd_Older, grp1n = n_Older, 
                   grp2m = mean_Younger,  grp2sd = sd_Younger,  grp2n = n_Younger, 
                   es.type = 'g')[1][[1]], 
       std_err = esc_mean_sd(grp1m = mean_Older, grp1sd = sd_Older, grp1n = n_Older, 
                                grp2m = mean_Younger,  grp2sd = sd_Younger,  grp2n = n_Younger, 
                                es.type = 'g')[2][[1]], 
       var = esc_mean_sd(grp1m = mean_Older, grp1sd = sd_Older, grp1n = n_Older, 
                            grp2m = mean_Younger,  grp2sd = sd_Younger,  grp2n = n_Younger, 
                            es.type = 'g')[3][[1]]
)

# only take mean from Liu 2016
liu<- dm[which(dm$conditionID == 'Liu 2016.Ln(k) mean'),]
dm <- dm[-which(dm$Study.Identifier == 'Liu 2016'),]
dm <- rbind(dm, liu)

## average effect size across multiple values within the same study 
average_within_study <- function(df, studyid) {
  x = df[which(df$Study.Identifier == studyid),] # pull out study of interest
  newmean <- cbind(x[1,1:18], t(colMeans(x[19:21]))) # average across estimates within same study
  dt <- df[-which(df$Study.Identifier == studyid),] # remove multiple estimates from df
  rbind(dt, newmean) # add new mean estimate to df
}

dm <- average_within_study(dm, 'Li 2013')
dm <- average_within_study(dm, 'Eppinger 2018')
dm <- average_within_study(dm, 'Whelan 2009')

# remove unneccesary columns
dm$mean_Older <- NULL
dm$mean_Younger <- NULL
dm$sd_Older <- NULL
dm$sd_Younger <- NULL
dm$conditionID <- NULL

# Reversals- Garza 2016, Sparrow 2018a, Sparrow 2018b, Li 2013 
#dm <- reverse_es(dm, 'Garza 2016')
dm <- reverse_es(dm, 'Sparrow 2018a')
dm <- reverse_es(dm, 'Sparrow 2018b Study 1')
dm <- reverse_es(dm, 'Sparrow 2018b Study 2')
dm <- reverse_es(dm, 'Li 2013')

## effect per decade - what about variance/se???
#dm$age_diff = dm$age_mean_Older - dm$age_mean_Younger
#dm$adj_effect_size <- (dm$yi/dm$age_diff) * 10 # calculate effect per year and then multiply by 10 for decade

write.csv(dm, here::here('output', 'extreme_group_table.csv'), row.names = FALSE)
