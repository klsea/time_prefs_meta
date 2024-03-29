# Calc correlation with age in raw data
# 6.12.20 KLS updated 6.29.20 with Lempert unpublished and Halfmann added

# load required packages
library(here)
library(tidyverse)

# load source functions

# set hard-coded variables
file <- 'cleaned.csv'
file1 <- 'Chao_2009_JDM.csv'
file2 <- 'Lee_2018_PRSB_reformatted.csv'
file3 <- 'LiYeetal_unpub.csv'
file4 <- "OHoraetal_Nature_2016.csv"
file5 <- 'Sissoetal_unpub_summary.csv'
file6 <- 'Lempert_unpub.csv'
file7 <- 'Halfmann_2013_Neuroscience.csv'

# load data
dt <- read.csv(here::here('output', file))
chao <- read.csv(here::here('data', file1))
lee <- read.csv(here::here('data', file2))
li <- read.csv(here::here('data', file3))
ohora <- read.csv(here::here('data', file4))
sisso <- read.csv(here::here('data', file5))
lempert <- read.csv(here::here('data', file6))
halfmann <- read.csv(here::here('data', file7))

# setup overall doc ####
names <- colnames(dt)
d1 <- data.frame(matrix(ncol = length(names), nrow = 7))
colnames(d1) <- names

# Chao 2009 ####
# SDR = subjective discount rate; higher SDR = greater discounting
d1$Study.Identifier[1] <- 'Chao 2009'
d1$Intervention[1] <- 'age'
d1$Year[1] <- 2009
d1$Design[1] <- 'continuous age'
d1$Incentive[1] <- 'hypothetical'
d1$Magnitude.of.Time.Delay[1] <- 'days' # changed from months to days in revision
d1$Measure[1] <- 'parameter'
d1$n[1] <- nrow(chao)
d1$age_mean[1] <- mean(chao$age)
d1$age_range[1] <- paste0(min(chao$age), ' - ', max(chao$age))
d1$age_sd[1] <-sd(chao$age)
d1$correlation[1] <- cor(chao[2:3])[1,2]
rm(chao)


# Lee 2018 ####
# k value - higher k value = greater discoutning
d1$Study.Identifier[2] <- 'Lee 2018'
d1$Intervention[2] <- 'age'
d1$Year[2] <- 2018
d1$Design[2] <- 'continuous age'
d1$Incentive[2] <- 'hypothetical'
d1$Magnitude.of.Time.Delay[2] <- 'days'
d1$Measure[2] <- 'parameter'
d1$n[2] <- nrow(lee)
d1$age_mean[2] <- mean(lee$age)
d1$age_range[2] <- paste0(min(lee$age), ' - ', max(lee$age))
d1$age_sd[2] <-sd(lee$age)
d1$correlation[2] <- cor(lee[c(3,5)], use = 'complete.obs')[1,2]
rm(lee)


# Li unpublished #### 
## beta-delta model; higher delta = higher discounting
d1$Study.Identifier[3] <- 'Li unpublished Study 2'
d1$Intervention[3] <- 'age'
d1$Year[3] <- NA
d1$Design[3] <- 'continuous age'
d1$Incentive[3] <- 'hypothetical'
d1$Magnitude.of.Time.Delay[3] <- 'days'
d1$Measure[3] <- 'parameter'
d1$n[3] <- nrow(li)
d1$age_mean[3] <- mean(li$age)
d1$age_range[3] <- paste0(min(li$age), ' - ', max(li$age))
d1$age_sd[3] <-sd(li$age)
d1$correlation[3] <- cor(li[c(4,19)], use = 'complete.obs')[1,2] # double check direction
rm(li)


# O'Hara 2016 ####
# k AUC = higher values = higher discounting?
ohora[ohora == ''] <- NA
ohora[ohora == '60+'] <- NA
ohora$age <- as.numeric(ohora$age)
d1$Study.Identifier[4] <- "O'Hora 2016"
d1$Intervention[4] <- 'age'
d1$Year[4] <- 2016
d1$Design[4] <- 'continuous age'
d1$Incentive[4] <- 'hypothetical'
d1$Magnitude.of.Time.Delay[4] <- 'years' #changed from days to years in revision based on Table 3
d1$Measure[4] <- 'parameter'
d1$n[4] <- nrow(ohora)
d1$age_mean[4] <- mean(ohora$age, na.rm = TRUE)
d1$age_range[4] <- paste0(min(ohora$age, na.rm = TRUE), ' - ', max(ohora$age, na.rm = TRUE))
d1$age_sd[4] <-sd(ohora$age, na.rm = TRUE)
d1$correlation[4] <- cor(ohora[c(2,3)], use = 'complete.obs')[1,2]
rm(ohora)


# Sisso unpublished ####
# Sisso, I. & Shayo, M. - When in Rome - The Effect of Financial Market Exposure on Maximizing Tendencies.
# higher values = greater discounting
d1$Study.Identifier[5] <- "Sisso unpublished"
d1$Intervention[5] <- 'age'
d1$Year[5] <- NA
d1$Design[5] <- 'continuous age'
d1$Incentive[5] <- 'hypothetical'
d1$Magnitude.of.Time.Delay[5] <- 'weeks'
d1$Measure[5] <- 'proportion'
d1$n[5] <- nrow(sisso)
d1$age_mean[5] <- mean(sisso$age, na.rm = TRUE)
d1$age_range[5] <- paste0(min(sisso$age, na.rm = TRUE), ' - ', max(sisso$age, na.rm = TRUE))
d1$age_sd[5] <-sd(sisso$age, na.rm = TRUE)
d1$correlation[5] <- cor(sisso[c(2,3)], use = 'complete.obs')[1,2]
rm(sisso)


# Lempert unpublished (psyarxiv) ####
# Normal controls from Lempert, Wolk & Kable
# higher discount rate = greater discounting
lempert <- lempert[-75,]
d1$Study.Identifier[6] <- "Lempert unpublished"
d1$Intervention[6] <- 'age'
d1$Year[6] <- NA
d1$Design[6] <- 'continuous age'
d1$Incentive[6] <- 'real' # changed to real in revision - incentive compatible on p. 19
d1$Magnitude.of.Time.Delay[6] <- 'days'
d1$Measure[6] <- 'parameter'
d1$n[6] <- nrow(lempert)
d1$age_mean[6] <- mean(lempert$age, na.rm = TRUE)
d1$age_range[6] <- paste0(min(lempert$age, na.rm = TRUE), ' - ', max(lempert$age, na.rm = TRUE))
d1$age_sd[6] <-sd(lempert$age, na.rm = TRUE)
d1$correlation[6] <- cor(lempert[c(2,3)], use = 'complete.obs')[1,2]
rm(lempert)


# Halfmann 2013 ####
# All participants from Halfmann 2013
# higher discount rate = greater discounting
d1$Study.Identifier[7] <- "Halfmann 2013"
d1$Intervention[7] <- 'age'
d1$Year[7] <- '2013'
d1$Design[7] <- 'continuous age'
d1$Incentive[7] <- 'hypothetical'
d1$Magnitude.of.Time.Delay[7] <- 'weeks'
d1$Measure[7] <- 'parameter'
d1$n[7] <- nrow(halfmann)
d1$age_mean[7] <- mean(halfmann$age, na.rm = TRUE)
d1$age_range[7] <- paste0(min(halfmann$age, na.rm = TRUE), ' - ', max(halfmann$age, na.rm = TRUE))
d1$age_sd[7] <-sd(halfmann$age, na.rm = TRUE)
d1$correlation[7] <- cor(halfmann[c(14,3)], use = 'complete.obs')[1,2]
rm(halfmann)

# add to existing and save ####
dt <- rbind(dt, d1)
write.csv(dt, here::here('output', 'complete.csv'), row.names = FALSE)
rm(file, file1, file2, file3, file4, file5, file6, file7, names, d1, dt)

