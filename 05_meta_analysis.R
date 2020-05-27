# Run basic meta-analysis and create forest plot
# 5.22.20 KLS

# load required packages
library(here)
library(tidyverse)
library(metafor)

# load source functions

# set hard-coded variables
file <- 'effect_sizes.csv'

# load data
dt <- read.csv(here::here('data', file), stringsAsFactors = FALSE)

# Put in alpha order by design
dt <- dt[order(dt$Design, dt$Study.Identifier),]

# Remove Eppinger 2012
dt <- dt[which(dt$Study.Identifier != "Eppinger 2012"),] # wrong data 

# Random Effects model
REM <- rma(yi, vi, data = dt, digits = 3, slab=Study.Identifier, method = "REML")
REM

# Funnel plot
forest(REM, 
       xlab = "Effect size", header="First Author and Year")



