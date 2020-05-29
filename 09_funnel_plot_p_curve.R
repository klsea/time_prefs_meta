# Create pcurve to check for pub bias
# 5.29.20 KLS

# load required packages
library(here)
library(dmetar) ## this package is not working on my computer

# load source functions

# set hard-coded variables
file <- 'effect_sizes.csv'

# load data
dt <- read.csv(here::here('data', file))

# Random Effects model
REM <- rma(adj_effect_size, adj_variance, data = dt, digits = 3, slab=Study.Identifier, method = "REML")

# Funnel plot
funnel(REM)

# pcurve
pcurve(REM)
