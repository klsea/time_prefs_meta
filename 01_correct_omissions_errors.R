# Calcs and correct errors/omissions in data extraction
# 6.4.2020 updated 6.17.20 

# load required packages
library(here)

# load source functions
#source(here::here('scr', 'conversion_functions.R'))

# set hard-coded variables
file <- 'covidence.csv'

# load data
dt <- read.csv(here::here('output', file))

# Errors ####

# Fix Study ID for Green 1994
dt[which(dt$Study.Identifier == 'GREEN 1994'), which(colnames(dt) == 'Study.Identifier')] <- 'Green 1994'

# Fix Tschernegg 2015 measure - correlation is taken from %SS decisions, not model parameters
dt[which(dt$Study.Identifier == 'Tschernegg 2015'), which(colnames(dt) == 'Measure')] <- 'proportion'

# Fix Boyle 2012 design
dt[which(dt$Study.Identifier == 'Boyle 2012'), which(colnames(dt) == 'Design')] <- 'continuous age'

# Fix Loceknhoff 2011 design
dt[which(dt$Study.Identifier == 'Löckenhoff 2011'), which(colnames(dt) == 'Design')] <- 'continuous age'

# Fix Buono 2015 design
dt[which(dt$Study.Identifier == 'Buono 2015'), which(colnames(dt) == 'Design')] <- 'extreme group'

# Omissions ####
# Fix Sanchez-Rodrigue 2018 incentive
dt[which(dt$Study.Identifier == 'Sanchez-Roige 2018'), which(colnames(dt) == 'Incentive')] <- 'hypothetical'

# Age Mean for Gollner 2018 (Data taken from Table 1) - note they claim 96 participants, but Table 1 adds up to 101
dt[which(dt$Study.Identifier == 'Gollner 2018'), which(colnames(dt) == 'age_mean')] <- (11.3*18 + 21.8*25 + 42.3*18 + 73.4*25 + 90.0*15)/(18+25+18+25+15)
                                                                                                                                          
# Age Mean for Johnson (Data taken from Table 5 - nonsmokers)
dt[which(dt$Study.Identifier == 'Johnson 2015'), which(colnames(dt) == 'age_mean')] <- (35.6*55 + 35.5*59 + 35.6*61)/(55+59+61)

# Age mean for Kirby 2002 - Table 4 27.4 Median - make a star in table with exception
dt[which(dt$Study.Identifier == 'Kirby 2002'), which(colnames(dt) == 'age_mean')] <- 27.4

# Age mean for Tschernegg 2015
dt[which(dt$Study.Identifier == 'Tschernegg 2015'), which(colnames(dt) == 'age_mean')] <- (24.5*51 + 27.15*19)/(51+19)

# Add Liu 2016 middle age group 
liuma <- dt[which(dt$Study.Identifier == 'Liu 2016' & dt$Intervention == 'Older'),]
liuma$Intervention <- 'Middle'
liuma$age_mean <- 43.27
liuma$age_sd <- 6.74
liuma$age_range <- '31-60'
liuma$n <- 745
liuma$mean <- c(-4.22, -3.93, -3.93, -3.64)
liuma$sd <- c(1.92, 1.76, 1.88, 1.80)
dt <- rbind(dt, liuma)

# Estimated means ####
# add assumed means for Buono 2015 & Garza 2016
dt[which(dt$Study.Identifier == 'Buono 2015'), ]$age_mean <- c(mean(seq(18,27)), mean(seq(45,55)))
dt[which(dt$Study.Identifier == 'Garza 2016'), ]$age_mean <- c(mean(seq(21,34)), mean(seq(35,49)), mean(seq(50,64)), mean(seq(65,76)))
dt[which(dt$Study.Identifier == 'Jimura 2011' & dt$Intervention == 'Younger'), ]$age_mean <- 20

# write data file
write.csv(dt, here::here('output', 'corrected.csv'), row.names = FALSE)
rm(dt, file, liuma)
                                                                                                                                          
                                                                                                                                          
                                                                                                                                          