# Make tables
# 5.29.20 KLS

# load required packages
library(here)
library(tidyverse)
library(sjPlot)

# load source functions

# set hard-coded variables

# load data
dc <- read.csv(here::here('output', 'continuous_table.csv'))
dm <- read.csv(here::here('output', 'extreme_group_table.csv'))
ds <- read.csv(here::here('output', 'extreme_group_stat_table.csv'))

# make combined data table for analysis ####
dt <- bind_rows(dm, ds)
dt$n <- dt$n_Older + dt$n_Younger
d0 <- bind_rows(dc[c(1:5, 9:13)], dt[c(1:5, 18, 14:17)])
write.csv(d0, here::here('output', 'effect_sizes.csv'), row.names = FALSE)

# pretty continous table for manuscript ####
dc$age_mean <- round(dc$age_mean, 2)
dc <- dc[order(dc$Study.Identifier), ]
dc[dc == ''] <- NA
dc[is.na(dc)] <- '--'
dc[which(dc$Study.Identifier == 'Kirby 2002'), which(colnames(dc) == 'age_mean')] <- '27.4*'
tab_df(dc[c(1, 3:9, 12, 10)], file = here::here('figs', 'continuous_table.html'))

# pretty group table for manuscript ####
dt <- dt[order(dt$Study.Identifier), ]
dt[dt == ''] <- NA
dt[is.na(dt)] <- '--'
dt<- dt[c(1:6, 8, 10, 12, 7, 9, 11, 13:17)]
dt$n <- dt$n_Older + dt$n_Younger
tab_df(dt[c(1, 3:14, 17, 15)], file = here::here('figs', 'extreme_group_table.html'))

# count number of participants
total_n <- sum(dc$n) + sum(dm$n_Younger + dm$n_Older)
rm(d0, dc, dm, ds, dt)
