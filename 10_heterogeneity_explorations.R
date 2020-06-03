# Run more heterogeneity analyses
# 6.2.20 KLS

# load required packages
library(here) 
library(tidyverse)
library(meta)
library(metafor)

# load source functions

# set hard-coded variables
file <- 'effect_sizes.csv'

# load data
dt <- read.csv(here::here('data', file), stringsAsFactors = FALSE)

# Put in alpha order by design
dt <- dt[order(dt$Design, dt$Study.Identifier),]

# Random Effects model - Knapp-Hartung (-Sidik-Jonkman) adjustment
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

# outlier analysis
find.outliers(m.hksj)
inf.analysis <- InfluenceAnalysis(x = m.hksj, random = TRUE)
summary(inf.analysis)
plot(inf.analysis, "influence")
plot(inf.analysis, "baujat")
plot(inf.analysis, "es")
plot(inf.analysis, "i2")

# GOSH plot # not working
m.rma <- rma(yi = m.hksj$TE, 
             sei = m.hksj$seTE,
             method = m.hksj$method.tau,
             test = "knha")
dat.gosh <- gosh(m.rma)
plot(dat.gosh, alpha= 0.1, col = "blue") # not working
gosh.diagnostics(dat.gosh) # not working
