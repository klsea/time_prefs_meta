# Run meta-regression analysis and bubble plots for older age in extreme-groups
# 1.21.21 KLS

# load required packages
library(here)
library(meta)
library(metafor)

# load source functions

# set hard-coded variables

# load data
dm <- read.csv(here::here('output', 'extreme_group_table.csv'))
ds <- read.csv(here::here('output', 'extreme_group_stat_table.csv'))
dt <- bind_rows(dm, ds)
dt$n <- dt$n_Older + dt$n_Younger
rm(dm,ds)

# Random Effects model - Knapp-Hartung (-Sidik-Jonkman) adjustment ####
m.cor <- metacor(cor = fishers_z, 
                 n = n, 
                 data = dt, 
                 studlab= Study.Identifier,
                 method.tau = 'SJ', 
                 sm = 'ZCOR')
m.cor

# Mean age older ####
age <- metareg(m.cor, age_mean_Older)
bubble(age, xlab = 'Mean Age of Older Group', col.line = 'blue')
