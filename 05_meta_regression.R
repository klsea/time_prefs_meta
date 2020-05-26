# Run meta-regression analysis and boxplots
# 5.22.20 KLS

# load required packages
library(here)
library(metafor)

# load source functions

# set hard-coded variables
file <- 'effect_sizes.csv'

# load data
dt <- read.csv(here::here('data', file))

# Put in alpha order by design
dt <- dt[order(dt$Design, dt$Study.Identifier),]

# Remove Eppinger 2012
dt <- dt[which(dt$Study.Identifier != "Eppinger 2012"),] # wrong data 

# Design
m1 <- rma(yi, vi, data = dt, mods = Design, digits = 3, slab=Study.Identifier, method = "REML")
m1
boxplot(yi ~ Design, data = dt)

# Incentive 
## Fix factors
levels(dt$Incentive)
dt$Incentive[which(dt$Incentive == 'real ')] <- 'real'
dt$Incentive[which(dt$Incentive == '')] <- 'hypothetical'
dt$Incentive <- factor(dt$Incentive)

m2 <- rma(yi, vi, data = dt, mods = Incentive, digits = 3, slab=Study.Identifier, method = "REML")
m2 
boxplot(yi ~ Incentive, data = dt)

# Delay
## Fix factors
levels(dt$Magnitude.of.Time.Delay)
dt$Magnitude.of.Time.Delay[which(dt$Magnitude.of.Time.Delay == 'months ')] <- 'months'
dt$Magnitude.of.Time.Delay <- ordered(dt$Magnitude.of.Time.Delay, levels = c('hours', 'days', 'weeks', 'months', 'years'))

m3 <- rma(yi, vi, data = dt, mods = Magnitude.of.Time.Delay, digits = 3, slab=Study.Identifier, method = "REML")
m3
boxplot(yi ~ Magnitude.of.Time.Delay, data = dt)

# Measure
## Fix factors
levels(dt$Measure)
dt$Measure[which(dt$Measure == 'parameter ')] <- 'parameter'
dt$Measure <- factor(dt$Measure)
m4 <- rma(yi, vi, data = dt, mods = Measure, digits = 3, slab=Study.Identifier, method = "REML")
m4
boxplot(yi ~ Measure, data = dt)
