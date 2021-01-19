# Convert to effect sizes from t or F vals
# 5.29.20 KLS
#6.20.20 CHANGE THIS SCRIPT

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

# Calculate effect sizes from statistics ####
dm <- dt[which(dt$Design == 'extreme group'),] # pull out means
ds <- dm[is.na(dm$sd),] # pull out no sd files

ds <- ds[c(1:2, 6, 8:10, 12:15, 19:20)]

ds <- pivot_wider(ds, 
                  id_cols = colnames(ds[c(1:6, 11:12)]), 
                  names_from = 'Intervention', 
                  values_from = c('n', 'age_mean', 'age_range', 'age_sd'))


# calculate effect size from t vals ####
ds <- ds[!is.na(ds$tvalue),]
ds <- mutate(ds, 
             fishers_z = esc_t(t = tvalue, grp1n = n_Older, grp2n = n_Younger, es.type = "r")[8][[1]], 
             var_fishers_z = 1 / ((n_Older + n_Younger) - 3)
             )

# remove unnecessary columns
ds$tvalue <- NULL
ds$Fvalue <- NULL
ds$df <- NULL

## effect per decade ####
#ds$adj_effect_size <- ds$effect_size * 10 # calculate effect per year and then multiply by 10 for decade

# Reversals ####
ds <- reverse_es(ds, 'Green 1994')

write.csv(ds, here::here('output', 'extreme_group_stat_table.csv'), row.names = FALSE)
rm(dm, ds, dt, file, reverse_es)
