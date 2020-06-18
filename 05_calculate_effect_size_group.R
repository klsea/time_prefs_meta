# Convert to effect sizes using esc package
# 6.2.2020 KLS
# 6.18.20 CHANGE THIS SCRIPT TO D

# load required packages
library(here)
library(tidyverse)
library(esc)

# load source functions
source(here::here('scr', 'reverse_es.R'))

# set hard-coded variables
file <- 'cleaned.csv'

# load data
dt <- read.csv(here::here('output', file))

# Calculation for two-group papers ####
# make interaction term for study + conditions
dt$conditionID <- interaction(dt$Study.Identifier, dt$condition)

# Calculate effect sizes for extreme group designs
dm <- dt[which(dt$Design == 'extreme group'),] # pull out means
ds <- dm[is.na(dm$sd),]
dm <- dm[!is.na(dm$sd),]
dm <- dm[c(1:2, 6, 8:15, 17:18, 21)]

### Temporarily remove Garza 2016 and Liu 2016 because missing age group means
garza <- dm[c(grep('Garza', dm$Study.Identifier)),]
liu <- dm[c(grep('Liu', dm$Study.Identifier)),]
dm <- dm[-c(grep('Garza', dm$Study.Identifier)),]
dm <- dm[-c(grep('Liu', dm$Study.Identifier)),]

## Calculate effect sizes
dm <- pivot_wider(dm, id_cols = colnames(dm[c(1:6, 14)]), names_from = 'Intervention', 
                  values_from = c('mean', 'sd', 'n', 'age_mean', 'age_range', 'age_sd'))
dm <- mutate(dm, effect_size = esc_mean_sd(grp1m = mean_Older, grp1sd = sd_Older, grp1n = n_Older, 
                   grp2m = mean_Younger,  grp2sd = sd_Younger,  grp2n = n_Younger, es.type = 'r')[1][[1]], 
       std_err = esc_mean_sd(grp1m = mean_Older, grp1sd = sd_Older, grp1n = n_Older, 
                                grp2m = mean_Younger,  grp2sd = sd_Younger,  grp2n = n_Younger, es.type = 'r')[2][[1]], 
       var = esc_mean_sd(grp1m = mean_Older, grp1sd = sd_Older, grp1n = n_Older, 
                            grp2m = mean_Younger,  grp2sd = sd_Younger,  grp2n = n_Younger, es.type = 'r')[3][[1]]
)

# Calculate for multi-group papers ####
age_group_comparisons <- function (data, oldergrp, youngergrp, name){ 
  # data = data frame with just data from this study
  # row containing data from the older group  row containing data from the younger group
  dt <- data[c(oldergrp, youngergrp),]
  dt$Intervention <- c('Older', 'Younger')
  dt <- pivot_wider(dt, id_cols = colnames(dt[c(1:6, 14)]), names_from = 'Intervention', 
                    values_from = c('mean', 'sd', 'n', 'age_mean', 'age_range', 'age_sd'))
  dt <- mutate(dt, effect_size = esc_mean_sd(grp1m = mean_Older, grp1sd = sd_Older, grp1n = n_Older, 
                                             grp2m = mean_Younger,  grp2sd = sd_Younger,  grp2n = n_Younger, es.type = 'r')[1][[1]], 
               std_err = esc_mean_sd(grp1m = mean_Older, grp1sd = sd_Older, grp1n = n_Older, 
                                     grp2m = mean_Younger,  grp2sd = sd_Younger,  grp2n = n_Younger, es.type = 'r')[2][[1]], 
               var = esc_mean_sd(grp1m = mean_Older, grp1sd = sd_Older, grp1n = n_Older, 
                                 grp2m = mean_Younger,  grp2sd = sd_Younger,  grp2n = n_Younger, es.type = 'r')[3][[1]]
  )
  dt$conditionID <- name
  return(dt)
}

## Liu 2016 ####
liu <- liu[which(liu$conditionID == 'Liu 2016.Ln(k) mean'),] # take the mean from paper and get rid of others.
liu1 <- age_group_comparisons(liu, 1, 3, 'OAvMA')
liu2 <- age_group_comparisons(liu, 3, 2, 'MAvYA')
liu <- rbind(liu1, liu2)
dm <- rbind(dm, liu)
rm(liu, liu1, liu2)

## Garza 2016 ####
garza1 <- age_group_comparisons(garza, 4, 3, 'OAvMA2')
garza2 <- age_group_comparisons(garza, 3, 2, 'MA2vMA1')
garza3 <- age_group_comparisons(garza, 2, 1, 'MA2vYA')
garza <- rbind(garza1, garza2)
garza <- rbind(garza, garza3)
dm <- rbind(dm, garza)
rm(garza, garza1, garza2, garza3)

## average effect size across multiple values within the same study ####
average_within_study <- function(df, studyid) {
  x = df[which(df$Study.Identifier == studyid),] # pull out study of interest
  newmean <- cbind(x[1,1:18], t(colMeans(x[19:21]))) # average across estimates within same study
  dt <- df[-which(df$Study.Identifier == studyid),] # remove multiple estimates from df
  rbind(dt, newmean) # add new mean estimate to df
}

dm <- average_within_study(dm, 'Li 2013')
dm <- average_within_study(dm, 'Eppinger 2018')
dm <- average_within_study(dm, 'Whelan 2009')
dm <- average_within_study(dm, 'Liu 2016')
dm <- average_within_study(dm, 'Garza 2016')

# remove unneccesary columns
dm$mean_Older <- NULL; dm$mean_Younger <- NULL; dm$sd_Older <- NULL; dm$sd_Younger <- NULL; dm$conditionID <- NULL

## effect per decade ####
dm$adj_effect_size <- dm$effect_size * 10 # calculate effect per year and then multiply by 10 for decade

# Reversals  ####
dm <- reverse_es(dm, 'Garza 2016')
dm <- reverse_es(dm, 'Li 2013')
dm <- reverse_es(dm, 'Sparrow 2018a')
dm <- reverse_es(dm, 'Sparrow 2018b Study 1')
dm <- reverse_es(dm, 'Sparrow 2018b Study 2')

write.csv(dm, here::here('output', 'extreme_group_table.csv'), row.names = FALSE)
rm(dm, ds, dt, file, average_within_study, reverse_es, age_group_comparisons)
