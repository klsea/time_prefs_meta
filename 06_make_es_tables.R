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

# pretty continous table for manuscript
dc <- dc[order(dc$Study.Identifier), ]
tab_df(dc[c(1, 3:10)], file = here::here('figs', 'continuous_table.html'))
#dc$raw_effect_size <- dc$yi
#dc$raw_variance <- dc$vi
#tab_df(dc[c(1,3:8, 13:14, 11:12)], file = here::here('figs', 'continuous_table.html'))

# pretty group table for manuscript
dt <- bind_rows(dm, ds)
dt <- dt[order(dt$Study.Identifier), ]
tab_df(dt[c(1, 3:13)], file = here::here('figs', 'extreme_group_table.html'))
#dt$raw_effect_size <- dt$yi
#dt$raw_variance <- dt$vi
#tab_df(dt[c(1, 3:7, 17:18, 15:16)], file = here::here('figs', 'extreme_group_table.html'))

# make combined data table for analysis
d0 <- bind_rows(dc[c(1:5, 9:11)], dt[c(1:5, 12:14)])
#d0 <- bind_rows(dc[c(1:5, 11:14)], dt[c(1:5, 15:18)])

write.csv(d0, here::here('data', 'effect_sizes.csv'), row.names = FALSE)

