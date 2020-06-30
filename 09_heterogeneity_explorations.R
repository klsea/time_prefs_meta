# Run more heterogeneity analyses
# 6.2.20 KLS

# load required packages
library(here) 
library(tidyverse)
library(meta)
library(metafor)

# load source functions

# set hard-coded variables
file <- 'cor_model.RDS'

# load data
m.cor <- readRDS(here::here('output', file))

# outlier analysis ####
find.outliers(m.cor)
inf.analysis <- InfluenceAnalysis(x = m.cor, random = TRUE)
summary(inf.analysis)
#plot(inf.analysis, "influence")
#plot(inf.analysis, "baujat")
#plot(inf.analysis, "es")
#plot(inf.analysis, "i2")

rm(dt, inf.analysis, m.cor, file)
