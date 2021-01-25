# Run meta-regression analysis and bubble plots for older age in extreme-groups
# 1.24.21 KLS

# load required packages
library(here)
library(meta)
library(metafor)
library(brms)

# load source functions

# set hard-coded variables
file <- 'effect_sizes.csv'
priors <- c(prior(normal(0,1), class = Intercept), 
            prior(cauchy(0,0.5), class = sd))

# load data
dt <- read.csv(here::here('output', file))

# run meta
m.brm <- brm(fishers_z|se(var_fishers_z) ~ 1  + ( 1 | Study.Identifier), 
              data = dt, 
              prior = priors, 
              iter = 6000, 
              control = list(max_treedepth = 18))

# check convergence
pp_check(m.brm) # look at plot to see if replications are similar to observed data
summary(m.brm) # look for rhat values to be less than 1.01

# save model
saveRDS(m.brm, here::here('output', 'bayesian_model.rds'))
