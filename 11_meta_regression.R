# Run meta-regression analysis and bubble plots
# 6.2.20 KLS

# load required packages
library(here)
library(meta)
library(metafor)

# load source functions

# set hard-coded variables
file <- 'effect_sizes.csv'

# load data
dt <- read.csv(here::here('output', file))

# Put in alpha order by design
dt <- dt[order(dt$Design, dt$Study.Identifier),]

# Fix factors ####
## Incentive
levels(dt$Incentive)
dt$Incentive[which(dt$Incentive == 'real ')] <- 'real'
dt$Incentive[which(dt$Incentive == '')] <- 'hypothetical'
dt$Incentive[which(dt$Incentive == 'Hypothetical')] <- 'hypothetical'
dt$Incentive[which(dt$Incentive == 'hypothetical/real')] <- 'real'
dt$Incentive <- factor(dt$Incentive)

## Magnitude
levels(dt$Magnitude.of.Time.Delay)
dt$Magnitude.of.Time.Delay[which(dt$Magnitude.of.Time.Delay == 'months ')] <- 'months'
dt$Magnitude.of.Time.Delay[which(dt$Magnitude.of.Time.Delay == 'Weeks')] <- 'weeks'
dt$Magnitude.of.Time.Delay[which(dt$Magnitude.of.Time.Delay == 'year')] <- 'years'
dt$Magnitude.of.Time.Delay[which(dt$Magnitude.of.Time.Delay == 'days/weeks/months/years')] <- 'years'
dt$Magnitude.of.Time.Delay[which(dt$Magnitude.of.Time.Delay == 'months/years')] <- 'years'
dt$Magnitude.of.Time.Delay[which(dt$Magnitude.of.Time.Delay == 'weeks/months/years')] <- 'years'
dt$Magnitude.of.Time.Delay <- ordered(dt$Magnitude.of.Time.Delay, levels = c('hours', 'days', 'weeks', 'months', 'years'))
dt$Delay <- dt$Magnitude.of.Time.Delay

## Measure
levels(dt$Measure)
dt$Measure[which(dt$Measure == 'Parameter')] <- 'parameter'
dt$Measure[which(dt$Measure == 'parameter ')] <- 'parameter'
dt$Measure <- factor(dt$Measure)

# Random Effects model - Knapp-Hartung (-Sidik-Jonkman) adjustment ####
m.cor <- metacor(cor = fishers_z, 
                 n = n, 
                 data = dt, 
                 studlab= Study.Identifier,
                 method.tau = 'SJ', 
                 sm = 'ZCOR')

# Design ####
design <- metareg(m.cor, Design)
bubble(design, xlab = 'Design', col.line = 'blue')

# Incentive ####
incent <- metareg(m.cor, Incentive)
bubble(incent, xlab = 'Incentive', col.line = 'blue')

# Delay ####
delay <- metareg(m.cor, Delay)
bubble(delay, xlab = 'Delay', col.line = 'blue')

# Measure ####
measure <- metareg(m.cor, Measure)
bubble(measure, xlab = 'Measure', col.line = 'blue')

#rm(dt, m.cor, design, incent, delay, measure, file)
