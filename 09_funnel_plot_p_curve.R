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
meta::funnel(m.hksj, xlab="Hedges' g", 
             contour = c(.95,.975,.99),
             col.contour=c("darkblue","blue","lightblue")) +
  legend(1.4, 0, c("p < 0.05", "p<0.025", "< 0.01"),bty = "n",
         fill=c("darkblue","blue","lightblue"))

# Eggers test
eggers.test(x = m.hksj)

# pcurve
pcurve(m.hksj)
