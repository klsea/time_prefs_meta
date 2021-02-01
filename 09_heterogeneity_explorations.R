# Run more heterogeneity analyses
# 6.2.20 KLS

#install dmetar package
install.packages("devtools")
devtools::install_github("MathiasHarrer/dmetar")
devtools::install("~/Path/To/The/Folder/dmetar-master")

# load required packages
library(here) 
library(tidyverse)
library(meta)
library(metafor)
library(dmetar)

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
