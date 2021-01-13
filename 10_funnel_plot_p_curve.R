# Create pcurve to check for pub bias
# 5.29.20 KLS

# load required packages
library(here)
library(meta)
library(metafor)
library(dmetar) 

# load source functions

# set hard-coded variables
file <- 'cor_model.RDS'

# load data
m.cor <- readRDS(here::here('output', file))

# Contor funnel plot 1
png(file = 'figs/funnelplot.png', width = 800, height = 800) 
meta::funnel(m.cor, xlim = c(-1.5,.5),
             contour = c(.95, .975, .99),
             col.contour=c("darkblue","blue","lightblue"))  
  legend(-1.5, 0, c("p < 0.05", "p<0.025", "< 0.01"), bty = "n",
         fill=c("darkblue","blue","lightblue"))
dev.off() 

# Contor funnel plot 2
png(file = 'figs/funnelplot2.png', width = 800, height = 800) 
meta::funnel(m.cor, xlim = c(-1.5,.5),
             contour = c(.95, .975, .99),
             col.contour=c("gray35","gray55","gray75"))  
legend(-1.5, 0, c("p < 0.05", "p<0.025", "< 0.01"), bty = "n",
       fill=c("gray35","gray55","gray75"))
dev.off() 

# Regular funnel plot
png(file = 'figs/funnelplot3.png', width = 800, height = 800) 
meta::funnel(m.cor, xlim = c(-1.5,.5))  
dev.off() 

# for presentation
ggsave(file = here::here('figs', 'funnelplot.svg'), plot = meta::funnel(m.cor, xlim = c(-1.5,.5)), width = 5, height = 5)


# Eggers test ####
eggers.test(x = m.cor)

# pcurve analysis ####
png(file = 'figs/pcurve.png', width = 500, height = 500) 
pcurve(m.cor)
dev.off() 

pcurve(m.cor, effect.estimation = TRUE, N = m.cor$n, dmin = 0, dmax = 1)

# for presentation
ggsave(file = here::here('figs', 'pcurve.svg'), plot = pcurve(m.cor), width = 5, height = 5)


rm(m.cor, file)
