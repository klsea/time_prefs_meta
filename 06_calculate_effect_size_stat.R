# Convert to effect sizes from t or F vals
# 5.29.20 KLS

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
dt1 <- ds[!is.na(ds$tvalue),]
dt1$effect_size <- esc_t(t = dt1$tvalue, grp1n = dt1$n_Older, grp2n = dt1$n_Younger, es.type = "r")[1][[1]]
dt1$std_err <- esc_t(t = dt1$tvalue, grp1n = dt1$n_Older, grp2n = dt1$n_Younger, es.type = "r")[2][[1]]
dt1$var <- esc_t(t = dt1$tvalue, grp1n = dt1$n_Older, grp2n = dt1$n_Younger, es.type = "r")[3][[1]]

# concatenate data tables
ds <- dt1

# remove unnecessary columns
ds$tvalue <- NULL
ds$Fvalue <- NULL
ds$df <- NULL

## effect per decade ####
ds$adj_effect_size <- ds$effect_size * 10 # calculate effect per year and then multiply by 10 for decade

# Reversals ####
ds <- reverse_es(ds, 'Green 1994')

write.csv(ds, here::here('output', 'extreme_group_stat_table.csv'), row.names = FALSE)
rm(dm, dt1, ds, dt, file, reverse_es)
