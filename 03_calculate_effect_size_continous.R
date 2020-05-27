# Convert to effect sizes
# 5.13.20 KLS

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

# Calculate effect sizes for continuous age designs
dc <- dt[which(dt$Design == 'continuous age'),] # pull out correlational studies
dc <- dc[!is.na(dc$correlation),] # remove incomplete studies
dc <- escalc(measure ='ZCOR', ri=correlation, ni=n, data=dc) # yi = effect size, vi = sampling variances
dc <- dc[c(1, 6, 8:12, 14, 23:25)]

## average across multiple values within the same study
hampton <- dc[which(dc$Study.Identifier == 'Hampton 2018'),]
hampton <- cbind(hampton[1,1:7], t(colMeans(hampton[8:9])))
johnson <- dc[which(dc$Study.Identifier == 'Johnson 2015'),]
johnson <- cbind(johnson[1,1:7], t(colMeans(johnson[8:9])))

## remove studies with multiple condition rows and replace with study means
dc <- dc[-which(dc$Study.Identifier == 'Hampton 2018'),]
dc <- rbind(dc, hampton)
dc <- dc[-which(dc$Study.Identifier == 'Johnson 2015'),]
dc <- rbind(dc, johnson)
rm(hampton, johnson)

## remove term for study + conditions
dc <- dc[-7]

write.csv(dc, here::here('figs', 'continuous_table.csv'), row.names = FALSE)



