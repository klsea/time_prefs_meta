# Convert to effect sizes
# 5.13.20 

# load required packages
library(here)
library(tidyverse)
library(metafor)

# load source functions

# set hard-coded variables
file <- 'covidence.csv'

# load data
dt <- read.csv(here::here('data', file))

# Calculate effect sizes for age
dc <- dt[which(dt$Design == 'continuous age'),] # pull out correlations
dc <- escalc(measure ='ZCOR', ri=correlation, ni=n, data=dc) # yi = effect size, vi = sampling variances

dm <- dt[which(dt$Design == 'extreme group'),] # pull out means
dm <- dm[!is.na(dm$sd),]

dm1 <- spread(dm[1:12, c(1, 2, 6, 18)], Intervention, mean)
colnames(dm1) <- c('Study.Identifier', 'Design', paste0('M_', colnames(dm1[,3:ncol(dm1)])))

dm2 <- spread(dm[1:12, c(1, 2, 6, 19)], Intervention, sd)
colnames(dm2) <- c('Study.Identifier', 'Design', paste0('SD_', colnames(dm2[,3:ncol(dm2)])))

dm3 <- spread(dm[1:12, c(1, 2, 6, 14)], Intervention, n)
colnames(dm3) <- c('Study.Identifier', 'Design', paste0('N_', colnames(dm3[,3:ncol(dm3)])))

dm4 <- merge(dm1, dm2,  by = 'Study.Identifier')
dm5 <- merge(dm4, dm3, by = 'Study.Identifier')

rm(dm1,dm2,dm3, dm4)

dm <- escalc(measure = 'SMD', n1i = N_Older, n2i = N_Younger, m1i = M_Older, m2i = M_Younger, sd1i = SD_Older, sd2i = SD_Younger, data=dm5)
