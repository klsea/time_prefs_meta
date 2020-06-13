# Create pcurve to check for pub bias
# 5.29.20 KLS

# load required packages
library(here)
library(meta)
library(metafor)
library(dmetar) 

# load source functions

# set hard-coded variables
file <- 'hksj_model.RDS'

# load data
m.hksj <- readRDS(here::here('output', file))

# Contor funnel plot - throwing an error ####
png(file = 'figs/funnelplot.png', width = 1000, height = 800) 
meta::funnel(m.hksj, xlab="Hedges' g", studlab = TRUE, xlim = c(-17,7),
             contour = c(.95, .975, .99),
             col.contour=c("darkblue","blue","lightblue"))  
  legend(-15, 0, c("p < 0.05", "p<0.025", "< 0.01"), bty = "n",
         fill=c("darkblue","blue","lightblue"))
dev.off() 

# Eggers test ####
eggers.test(x = m.hksj)

# pcurve analysis ####
png(file = 'figs/pcurve.png', width = 500, height = 500) 
pcurve(m.hksj)
dev.off() 

rm(m.hksj, file)
