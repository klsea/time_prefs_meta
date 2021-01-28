# Run bayesian meta-analysis using bayesmeta
# 1.28.21 KLS

# load required packages
library(here)
library(meta)
library(metafor)
library(ggplot2)
library(bayesmeta)

# load source functions

# set hard-coded variables
file <- 'effect_sizes.csv'
 
# load data
dt <- read.csv(here::here('output', file))

# run meta
m1 <- bayesmeta(y = dt[, "fishers_z"], sigma = sqrt(dt[, "var_fishers_z"]), labels = dt[, "Study.Identifier"], 
                mu.prior.mean = 0, mu.prior.sd = 4, tau.prior = function(t) dhalfnormal(t, scale = 0.5))

