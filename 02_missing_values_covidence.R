# Calculate missing values 
# 3.20.20 updated 6.9.20

# load required packages
library(here)

# load source functions
#source(here::here('scr', 'conversion_functions.R'))

# set hard-coded variables
file <- 'corrected.csv'

# load data
dt <- read.csv(here::here('data', file))

# Eppinger 2018 add means/ses for both Sessions
# used https://automeris.io/WebPlotDigitizer/
eppinger <- dt[which(dt$Study.Identifier == 'Eppinger 2018'),]
dt <- dt[-which(dt$Study.Identifier == 'Eppinger 2018'),]
eppinger1 <- read.csv(here::here('data', 'eppinger2018S1plotdigit.csv'), header = FALSE)
eppinger[, colnames(eppinger) == 'sd'] <- NA
sess1 <- eppinger
sess1[,which(colnames(sess1) == 'condition')] <- 'Session 1'
sess1[which(sess1$Intervention == 'Younger'), which(colnames(sess1) == 'mean')] <- eppinger1[1,2]
sess1[which(sess1$Intervention == 'Younger'), which(colnames(sess1) == 'se')] <- abs(eppinger1[1,2] - eppinger1[2,2])
sess1[which(sess1$Intervention == 'Older'), which(colnames(sess1) == 'mean')] <- eppinger1[3,2]
sess1[which(sess1$Intervention == 'Older'), which(colnames(sess1) == 'se')] <- abs(eppinger1[3,2] - eppinger1[4,2])

eppinger2 <- read.csv(here::here('data', 'eppinger2018S2plotdigit.csv'), header = FALSE)
low <- eppinger
low[,which(colnames(low) == 'condition')] <- 'Session 2: Low'
low[which(low$Intervention == 'Younger'), which(colnames(low) == 'mean')] <- eppinger2[1,2]
low[which(low$Intervention == 'Younger'), which(colnames(low) == 'se')] <- abs(eppinger2[1,2] - eppinger2[2,2])
low[which(low$Intervention == 'Older'), which(colnames(low) == 'mean')] <- eppinger2[5,2]
low[which(low$Intervention == 'Older'), which(colnames(low) == 'se')] <- abs(eppinger2[5,2] - eppinger2[6,2])

high <- eppinger
high[,which(colnames(high) == 'condition')] <- 'Session 2: High'
high[which(high$Intervention == 'Younger'), which(colnames(high) == 'mean')] <- eppinger2[3,2]
high[which(high$Intervention == 'Younger'), which(colnames(high) == 'se')] <- abs(eppinger2[3,2] - eppinger2[4,2])
high[which(high$Intervention == 'Older'), which(colnames(high) == 'mean')] <- eppinger2[7,2]
high[which(high$Intervention == 'Older'), which(colnames(high) == 'se')] <- abs(eppinger2[7,2] - eppinger2[8,2])
sess2 <- rbind(low, high)
eppinger <-rbind(sess1, sess2)
dt <- rbind(dt, eppinger)
rm(low, high, eppinger1, eppinger2, eppinger, sess2, sess1)

# Samanez-Larkin 2011 add tvalue
dt[which(dt$Study.Identifier == 'Samanez-Larkin 2011'), which(colnames(dt) == 'tvalue')] <- 0.20

# Sparrow 2018a pull in values rom plot digitizer and name Sparrow 2018a
sparrow <- read.csv(here::here('data', 'sparrow2018aplotdigit.csv'), header = FALSE)
dt$Study.Identifier <- as.character(dt$Study.Identifier)
dt[which(dt$correct_data_extracted == 'No mean or sd but we can extract from barplot'),
   which(colnames(dt) == 'Study.Identifier')] <- 'Sparrow 2018a' 
#dt$Study.Identifier <- factor(dt$Study.Identifier)  
dt[which(dt$Study.Identifier == 'Sparrow 2018a' & dt$Intervention == 'Younger'), which(colnames(dt) == 'mean')] <- sparrow[1,2]
dt[which(dt$Study.Identifier == 'Sparrow 2018a' & dt$Intervention == 'Younger'), which(colnames(dt) == 'se')] <- abs(sparrow[1,2] - sparrow[2,2])
dt[which(dt$Study.Identifier == 'Sparrow 2018a' & dt$Intervention == 'Older'), which(colnames(dt) == 'mean')] <- sparrow[3,2]
dt[which(dt$Study.Identifier == 'Sparrow 2018a' & dt$Intervention == 'Older'), which(colnames(dt) == 'se')] <- abs(sparrow[3,2] - sparrow[4,2])
rm(sparrow)

# Sparrow 2018b pull in values rom plot digitizer and name Sparrow 2018b
sparrow1 <- read.csv(here::here('data', 'sparrow2018bs1plotdigit.csv'), header = FALSE) # load Study 1data from plotdigitizer
sparrow2 <- read.csv(here::here('data', 'sparrow2018bS2plotdigit.csv'), header = FALSE)  # load Study 2data from plotdigitizer
sparrow <- dt[which(dt$Study.Identifier == 'Sparrow 2018'),]
sparrow[, colnames(sparrow) == 'tvalue'] <- NA
sparrow[, colnames(sparrow) == 'effect_size_d'] <- NA
dt <- dt[-which(dt$Study.Identifier == 'Sparrow 2018'),] # remove empty row from data frame

s1 <- sparrow
s1[,which(colnames(s1) == 'Study.Identifier')] <- 'Sparrow 2018b Study 1'
s1[which(s1$Intervention == 'Younger'), which(colnames(s1) == 'mean')] <- sparrow1[1,2]
s1[which(s1$Intervention == 'Younger'), which(colnames(s1) == 'se')] <- abs(sparrow1[1,2] - sparrow1[2,2])
s1[which(s1$Intervention == 'Older'), which(colnames(s1) == 'mean')] <- sparrow1[3,2]
s1[which(s1$Intervention == 'Older'), which(colnames(s1) == 'se')] <- abs(sparrow1[3,2] - sparrow1[4,2])

s2 <- sparrow
s2[,which(colnames(s2) == 'Study.Identifier')] <- 'Sparrow 2018b Study 2'
s2[which(s2$Intervention == 'Younger'), which(colnames(s2) == 'age_mean')] <- 20.84
s2[which(s2$Intervention == 'Younger'), which(colnames(s2) == 'age_sd')] <- 2.67
s2[which(s2$Intervention == 'Younger'), which(colnames(s2) == 'n')] <- 31
s2[which(s2$Intervention == 'Younger'), which(colnames(s2) == 'age_range')] <- '18-30'
s2[which(s2$Intervention == 'Older'), which(colnames(s2) == 'age_mean')] <- 71.35
s2[which(s2$Intervention == 'Older'), which(colnames(s2) == 'age_sd')] <- 6.96
s2[which(s2$Intervention == 'Older'), which(colnames(s2) == 'n')] <- 23
s2[which(s2$Intervention == 'Older'), which(colnames(s2) == 'age_range')] <- '65-97'
s2[which(s2$Intervention == 'Younger'), which(colnames(s2) == 'mean')] <- sparrow1[1,2]
s2[which(s2$Intervention == 'Younger'), which(colnames(s2) == 'se')] <- abs(sparrow1[1,2] - sparrow1[2,2])
s2[which(s2$Intervention == 'Older'), which(colnames(s2) == 'mean')] <- sparrow1[3,2]
s2[which(s2$Intervention == 'Older'), which(colnames(s2) == 'se')] <- abs(sparrow1[3,2] - sparrow1[4,2])

s3 <- rbind(s1, s2)
dt <- rbind(dt, s3)
rm(s1, s2, s3, sparrow, sparrow1, sparrow2)

# calculate means and sds from Whelan 2009
whelan <- read.csv(here::here('data', 'Whelan2009data.csv'))
mean <- colMeans(whelan, na.rm = TRUE)
sd <- apply(whelan, 2, sd, na.rm =TRUE)

dt[which(dt$condition == '£100' & dt$Intervention == 'Younger'), which(colnames(dt) == 'mean')] <- mean[3][[1]]
dt[which(dt$condition == '£100' & dt$Intervention == 'Younger'), which(colnames(dt) == 'sd')] <- sd[3][[1]]
dt[which(dt$condition == '£100' & dt$Intervention == 'Older'), which(colnames(dt) == 'mean')] <- mean[5][[1]]
dt[which(dt$condition == '£100' & dt$Intervention == 'Older'), which(colnames(dt) == 'sd')] <- sd[5][[1]]
dt[which(dt$condition == '£1,000' & dt$Intervention == 'Younger'), which(colnames(dt) == 'mean')] <- mean[4][[1]]
dt[which(dt$condition == '£1,000' & dt$Intervention == 'Younger'), which(colnames(dt) == 'sd')] <- sd[4][[1]]
dt[which(dt$condition == '£1,000' & dt$Intervention == 'Older'), which(colnames(dt) == 'mean')] <- mean[6][[1]]
dt[which(dt$condition == '£1,000' & dt$Intervention == 'Older'), which(colnames(dt) == 'sd')] <- sd[6][[1]]
rm(whelan, mean, sd)

# convert rsquare to r - should be able to remove after getting rid of Read 2004
dt[which(dt$rsquare != 'NA'),]$correlation <- sqrt(dt[which(dt$rsquare != 'NA'),]$rsquare)
dt$rsquare <- NULL

# convert se to sd
dt[which(dt$se != 'NA'),]$sd <- dt[which(dt$se != 'NA'),]$se * sqrt(dt[which(dt$se != 'NA'),]$n)
dt$se <- NULL

# write data file
write.csv(dt, here::here('data', 'cleaned.csv'), row.names = FALSE)

