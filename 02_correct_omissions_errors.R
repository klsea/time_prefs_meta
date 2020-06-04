# Cals and correct errors/omissions in data extraction
# 6.4.202

# load required packages
library(here)

# load source functions
#source(here::here('scr', 'conversion_functions.R'))

# set hard-coded variables
file <- 'cleaned.csv'

# load data
dt <- read.csv(here::here('data', file))

# Fix Eppinger 2012 - SEs recorded as SDs
dt[which(dt$Study.Identifier == 'Eppinger 2012'), which(colnames(dt) == 'se')] <- dt[which(dt$Study.Identifier == 'Eppinger 2012'), which(colnames(dt) == 'sd')]
dt[which(dt$Study.Identifier == 'Eppinger 2012'), which(colnames(dt) == 'sd')] <- NA

# Fix Sanchez-Rodrigue 2018 incentive
dt[which(dt$Study.Identifier == 'Sanchez-Roige 2018'), which(colnames(dt) == 'Incentive')] <- 'hypothetical'

# Age Mean for Gollner 2018 (Data taken from Table 1) - note they claim 96 participants, but Table 1 adds up to 101
dt[which(dt$Study.Identifier == 'Gollner 2018'), which(colnames(dt) == 'age_mean')] <- (11.3*18 + 21.8*25 + 42.3*18 + 73.4*25 + 90.0*15)/(18+25+18+25+15)
                                                                                                                                          
# Age Mean for Johnson (Data taken from Table 5 - nonsmokers)
dt[which(dt$Study.Identifier == 'Johnson 2015'), which(colnames(dt) == 'age_mean')] <- (35.6*55 + 35.5*59 + 35.6*61)/(55+59+61)
### Can we estimate range from mean, sd?

# Age mean for Kirby 2002 - Table 4 27.4 Median - make a star in table with exception
dt[which(dt$Study.Identifier == 'Kirby 2002'), which(colnames(dt) == 'age_mean')] <- 27.4
  
# Age mean for Riemers 2009 - not available

# Age mean for Tschernegg 2015
dt[which(dt$Study.Identifier == 'Tschernegg 2015'), which(colnames(dt) == 'age_mean')] <- (24.5*51 + 27.15*19)/(51+19)

# Age mean for younger adults in Jimura 2011 - not available

dt$age_mean <- round(dt$age_mean, 2)

# write data file
write.csv(dt, here::here('data', 'cleaned.csv'), row.names = FALSE)
                                                                                                                                          
                                                                                                                                          
                                                                                                                                          