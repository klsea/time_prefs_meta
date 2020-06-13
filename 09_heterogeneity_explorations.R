# Run more heterogeneity analyses
# 6.2.20 KLS

# load required packages
library(here) 
library(tidyverse)
library(meta)
library(metafor)

# load source functions

# set hard-coded variables
file <- 'hksj_model.RDS'

# load data
m.hksj <- readRDS(here::here('output', file))

# outlier analysis ####
find.outliers(m.hksj)
inf.analysis <- InfluenceAnalysis(x = m.hksj, random = TRUE)
summary(inf.analysis)
plot(inf.analysis, "influence")
plot(inf.analysis, "baujat")
plot(inf.analysis, "es")
plot(inf.analysis, "i2")

# GOSH plot # not working ####
#m.rma <- rma(yi = m.hksj$TE, 
#             sei = m.hksj$seTE,
#             method = m.hksj$method.tau,
#             test = "knha")
#dat.gosh <- gosh(m.rma)
#plot(dat.gosh, alpha= 0.1, col = "blue") # not working
#gosh.diagnostics(dat.gosh) # not working

rm(dt, inf.analysis, m.hksj, file)
