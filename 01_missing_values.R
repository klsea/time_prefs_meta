# Calculate missing values 
# 3.20.20 

# load required packages
library(here)

# load source functions
source(here::here('scr', 'conversion_functions.R'))

# set hard-coded variables
file <- 'covidence.csv'

# load data
dt <- read.csv(here::here('data', file))

# convert rsquare to r
dt[which(dt$rsquare != 'NA'),]$correlation <- sqrt(dt[which(dt$rsquare != 'NA'),]$rsquare)
dt$rsquare <- NULL

# convert se to sd
dt[which(dt$se != 'NA'),]$sd <- dt[which(dt$se != 'NA'),]$se * sqrt(dt[which(dt$se != 'NA'),]$n)
dt$se <- NULL
