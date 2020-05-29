# Convert to effect sizes from t or F vals
# 5.29.20 KLS

# load required packages
library(here)
library(tidyverse)
library(esc)

# load source functions

# set hard-coded variables
file <- 'cleaned.csv'

# load data
dt <- read.csv(here::here('data', file))

# make interaction term for study + conditions
dt$conditionID <- interaction(dt$Study.Identifier, dt$condition)

# Calculate effect sizes from statistics
dm <- dt[which(dt$Design == 'extreme group'),] # pull out means
ds <- dm[is.na(dm$sd),] # pull out no sd files

ds <- ds[c(1:2, 6, 8:12, 14, 20:21, 23)]

ds <- pivot_wider(ds, 
                  id_cols = colnames(ds[c(1:6, 10:12)]), 
                  names_from = 'Intervention', 
                  values_from = c('n', 'age_mean', 'age_range'))


# calculate effect size from t vals
dt1 <- ds[!is.na(ds$tvalue),]
dt1$yi <- esc_t(t = dt1$tvalue, grp1n = dt1$n_Older, grp2n = dt1$n_Younger, es.type = "d")[1][[1]]
dt1$vi <- esc_t(t = dt1$tvalue, grp1n = dt1$n_Older, grp2n = dt1$n_Younger, es.type = "d")[3][[1]]

# calculate effect size from f vals
dt2 <- ds[!is.na(ds$Fvalue),]
dt2$yi <- esc_f(f = dt2$Fvalue, grp1n = dt2$n_Older, grp2n = dt2$n_Younger, es.type = "d")[1][[1]]
dt2$vi <- esc_f(f = dt2$Fvalue, grp1n = dt2$n_Older, grp2n = dt2$n_Younger, es.type = "d")[3][[1]]

## average across multiple values within the same study
dt2 <- cbind(dt2[1, c(1:14)], t(colMeans(dt2[15:16])))

# concatenate data tables
ds <- bind_rows(dt1, dt2)

ds$tvalue <- NULL
ds$Fvalue <- NULL
ds$conditionID <- NULL

## effect per decade
ds$age_diff = ds$age_mean_Older - ds$age_mean_Younger
ds$adj_effect_size <- (ds$yi/ds$age_diff) * 10 # calculate effect per year and then multiply by 10 for decade
ds$adj_variance <- (ds$vi/ds$age_diff) * 10 

write.csv(ds, here::here('output', 'extreme_group_stat_table.csv'), row.names = FALSE)
