# Create pcurve to check for pub bias
# 5.29.20 KLS

# load required packages
library(here)
library(meta)
library(dmetar) 

# load source functions

# set hard-coded variables
file <- 'effect_sizes.csv'

# load data
dt <- read.csv(here::here('data', file))

# Random Effects model
m.hksj <- metagen(TE = effect_size, 
                  seTE = std_err, 
                  data = dt, 
                  studlab= Study.Identifier,
                  comb.fixed = FALSE,
                  comb.random = TRUE, 
                  method.tau = 'SJ', 
                  hakn = TRUE, 
                  prediction = TRUE, 
                  sm = 'SMD')
m.hksj

# Funnel plot
meta::funnel(m.hksj)

# pcurve
pcurve(m.hksj)
