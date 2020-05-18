# Sort data from covidence
# 2.26.20 updated 3.20.20 more efficient script

# load required packages
library(here)
library(tidyverse)
library(magrittr)
library(janitor)
library(plyr)

# load source functions
source(here::here('scr', 'separate_conditions.R'))

# set hard-coded variables
file <- 'review_47118_extracted_data_csv_20200227082348.csv'

# load data
dt <- read.csv(here::here('data', file))

# find columns that contain *only* NA and eliminate
d1 <- dt[colSums(is.na(dt))/49 < 1]
rm(dt)

# combine columns with the same data
d2 <- d1 %>%
  mutate_all(as.character) %>%
  unite('correct_data_extracted', c(Correct.data.extracted.,Correct.Data.Extracted.), 
        sep ='', na.rm=TRUE) %>%
  unite('age_range', c(Baseline.Age.Range, Baseline.Age..Range, Baseline.Range, Baseline..Range), 
        sep = '', na.rm=TRUE) %>%
  unite('age_mean', c(Baseline.Age.Mean, Baseline.Mean, Baseline..Mean), 
        sep = '', na.rm=TRUE) %>%
  unite('age_sd', c(Baseline.Age.SD, Baseline.Sd, Baseline.SD), sep ='', na.rm=TRUE) %>%
  unite('df', c(Discounting.Baseline.df, Discounting.Baseline.df.1, Discounting.Baseline.df.2),
        sep = '', na.rm=TRUE) %>%
  unite('n', colnames(d1)[grep('Discounting.Baseline.N', colnames(d1))], 
        sep = '_', na.rm = TRUE) %>%
  unite('tvalue', colnames(d1)[grep('Discounting.Baseline.t.value', colnames(d1))], 
        sep='_', na.rm=TRUE) 

colnames(d2)[which(colnames(d2) == 'Discounting.Baseline.SE')] <- 'se'
colnames(d2)[which(colnames(d2) == 'Discounting.Baseline.F.value')] <- 'Fvalue'
colnames(d2)[which(colnames(d2) == 'Discounting.Baseline.effectsize..d.')] <- 'effect_size_d'

# Separate conditions for correlations
colnames(d2)[which(colnames(d2) == 'Discounting.Baseline.correlation')] <- 'correlation'

# Hampton 2018
hampton <- separate_conditions(d2, 'Hampton 2018', c(1, 7, 30, 180, 365), 'Discounting.Baseline.correlation')
colnames(hampton)[which(colnames(hampton) == 'measure')] <- 'correlation'
hampton$condition <- paste0(hampton$condition, ' days')
hampton['se'] <- NA; hampton['sd'] <- NA; hampton['Fvalue'] <- NA; hampton['mean'] <- NA; hampton['effect_size_d'] <- NA; hampton['rsquare'] <- NA
d2 <- d2[-which(d2$Study.Identifier == 'Hampton 2018'),] # Remove Hampton from d2 dataframe; will put back in later

# Johnson 2015
johnson <- separate_conditions(d2, 'Johnson 2015', c(1, 2), 'Discounting.Baseline.correlation.')
colnames(johnson)[which(colnames(johnson) == 'measure')] <- 'correlation'
johnson$condition <- paste0('$', johnson$condition)
johnson['se'] <- NA; johnson['sd'] <- NA; johnson['Fvalue'] <- NA; johnson['mean'] <- NA; johnson['effect_size_d'] <- NA; johnson['rsquare'] <- NA
d2 <- d2[-which(d2$Study.Identifier == 'Johnson 2015'),]

# Wolfe 2017 and Reimers 2009(for some reason Covidence recorded these in a different column)
dt <- d2[which(d2$Study.Identifier == 'Wolfe 2017'),] # isolate Wolfe data
dt <- dt[,colSums(is.na(dt)) == 0] # remove NA columns
colnames(dt)[which(colnames(dt) == 'Discounting.Baseline.correlation.3')] <- 'correlation'
dt['se'] <- NA; dt['sd'] <- NA; dt['Fvalue'] <- NA; dt['mean'] <- NA; dt['effect_size_d'] <- NA; dt['condition'] <- NA; dt['rsquare'] <- NA
wolfe <- dt; rm(dt)
d2 <- d2[-which(d2$Study.Identifier == 'Wolfe 2017'),]

reimers <- d2[which(d2$Study.Identifier == 'Reimers 2009'),] # isolate Reimers data
reimers <- reimers[,colSums(is.na(reimers)) == 0] # remove NA columns
colnames(reimers)[which(colnames(reimers) == 'Discounting.Baseline.correlation.3')] <- 'correlation'
reimers['se'] <- NA; reimers['sd'] <- NA; reimers['Fvalue'] <- NA; reimers['mean'] <- NA; reimers['effect_size_d'] <- NA; reimers['condition'] <- NA; reimers['rsquare'] <- NA
d2 <- d2[-which(d2$Study.Identifier == 'Reimers 2009'),]

# remove unneccesary correlation columns
d2 <- d2[-grep('Discounting.Baseline.correlation', colnames(d2))]

# Separate conditions for rsquare
# Read 2004
read <- separate_conditions(d2, 'Read 2004', c(1, 2, 3), 'Discounting.Baseline.rsquare')
colnames(read)[which(colnames(read) == 'measure')] <- 'rsquare'
read$condition <- c('0-3 year delays', '0-10 year delays', '7-10 year delays')
read['se'] <- NA; read['sd'] <- NA; read['Fvalue'] <- NA; read['mean'] <- NA; 
read['effect_size_d'] <- NA; read['correlation'] <- NA
d2 <- d2[-which(d2$Study.Identifier == 'Read 2004'),]

# remove unneccesary rsquare columns
d2 <- d2[-grep('Discounting.Baseline.rsquare', colnames(d2))]
d2$rsquare <- NA # create placeholder column

# Separate conditions for group means
colnames(d2)[which(colnames(d2) == 'Discounting.Baseline.mean')] <- 'mean'
colnames(d2)[which(colnames(d2) == 'Discounting.Baseline.SD.1')] <- 'sd'

# Li 2013
li_mean <- separate_conditions(d2, 'Li 2013', c(1, 2.1, 3, 4, 5), 'Discounting.Baseline.Mean')
colnames(li_mean)[which(colnames(li_mean) == 'measure')] <- 'mean'
li_mean <- li_mean[-grep('Discounting.Baseline.SD', colnames(li_mean))]
li_mean$condition <- revalue(li_mean$condition, c('1' = '$60, 4 mo', '2.1' = '$75, 3 mo', 
                                              '3' = '$55, 3 mo', '4' = '$115, 3 mo', 
                                              '5' = '$100, 12 mo'))

li_sd <- separate_conditions(d2, 'Li 2013', c(1, 2.1, 3.1, 4, 5), 'Discounting.Baseline.SD')
colnames(li_sd)[which(colnames(li_sd) == 'measure')] <- 'sd'
li_sd <- li_sd[-grep('Discounting.Baseline.Mean', colnames(li_sd))]
li_sd$condition <- revalue(li_sd$condition, c('1' = '$60, 4 mo', '2.1' = '$75, 3 mo', 
                                          '3.1' = '$55, 3 mo', '4' = '$115, 3 mo', 
                                          '5' = '$100, 12 mo'))
li <- merge(li_mean, li_sd)
li['se'] <- NA; li['Fvalue'] <- NA; li['effect_size_d'] <- NA; li['correlation'] <- NA; li['rsquare'] <- NA
rm(li_mean, li_sd)
d2 <- d2[-which(d2$Study.Identifier == 'Li 2013'),]

# Liu 2016
liu_mean <- separate_conditions(d2, 'Liu 2016', c(1.2, 2.2, 3.1, 4.1), 'Discounting.Baseline.Mean')
colnames(liu_mean)[which(colnames(liu_mean) == 'measure')] <- 'mean'
liu_mean <- liu_mean[-grep('Discounting.Baseline.SD', colnames(liu_mean))]
liu_mean$condition <- revalue(liu_mean$condition, c('1.2' = 'Ln(k) small', '2.2' = 'Ln(k) medium', 
                                              '3.1' = 'Ln(k) large', '4.1' = 'Ln(k) mean'))

liu_sd <- separate_conditions(d2, 'Liu 2016', c(1.1, 2.2, 3, 4.1), 'Discounting.Baseline.SD')
colnames(liu_sd)[which(colnames(liu_sd) == 'measure')] <- 'sd'
liu_sd <- liu_sd[-grep('Discounting.Baseline.Mean', colnames(liu_sd))]
liu_sd$condition <- revalue(liu_sd$condition, c('1.1' = 'Ln(k) small', '2.2' = 'Ln(k) medium', 
                                          '3' = 'Ln(k) large', '4.1' = 'Ln(k) mean'))
liu <- merge(liu_mean, liu_sd)
liu['se'] <- NA; liu['Fvalue'] <- NA; liu['effect_size_d'] <- NA; 
liu['correlation'] <- NA; liu['rsquare'] <- NA
rm(liu_mean, liu_sd)
d2 <- d2[-which(d2$Study.Identifier == 'Liu 2016'),]

# Green 1994 (no standard devs)
green <- separate_conditions(d2, 'GREEN 1994', c('1.000.', '10.000.'), 'Discounting.Baseline.Mean..')
colnames(green)[which(colnames(green) == 'measure')] <- 'mean'
green$condition <- revalue(green$condition, c('1.000.' = '$1,000', '10.000.' = '$10,000'))
green['se'] <- NA; green['Fvalue'] <- NA; green['effect_size_d'] <- NA; 
green['correlation'] <- NA; green['rsquare'] <- NA; green['sd'] <- NA
d2 <- d2[-which(d2$Study.Identifier == 'GREEN 1994'),]

# Whelan 2009 (no standard devs)
whelan <- separate_conditions(d2, 'Whelan 2009', c('1.1', '2'), 'Discounting.Baseline.Mean')
colnames(whelan)[which(colnames(whelan) == 'measure')] <- 'mean'
whelan$condition <- revalue(whelan$condition, c('1.1' = '£100', '2' = '£1,000'))
whelan['se'] <- NA; whelan['Fvalue'] <- NA; whelan['effect_size_d'] <- NA;
whelan['correlation'] <- NA; whelan['rsquare'] <- NA; whelan['sd'] <- NA
d2 <- d2[-which(d2$Study.Identifier == 'Whelan 2009'),]

# Jimura 2011 (for some reason Covidence recorded this in a different column)
jimura <- d2[which(d2$Study.Identifier == 'Jimura 2011'),] # isolate Jimura data
jimura <- jimura[,colSums(is.na(jimura)) == 0] # remove NA columns
colnames(jimura)[which(colnames(jimura) == 'Discounting.Baseline.Mean')] <- 'mean'
jimura['se'] <- NA; jimura['Fvalue'] <- NA; jimura['effect_size_d'] <- NA; jimura['condition'] <- NA; 
jimura['correlation'] <- NA; jimura['rsquare'] <- NA; jimura['sd'] <- NA
d2 <- d2[-which(d2$Study.Identifier == 'Jimura 2011'),]

# remove unneccesary mean and sd columns
d2 <- d2[-grep('Discounting.Baseline.Mean', colnames(d2))]
d2 <- d2[-grep('Discounting.Baseline.SD', colnames(d2))]

# Put it all back together
d2$condition <- NA # add empty condition column
order <- colnames(d2)
order <- order[c(1:13, 15, 24, 16, 19:23, 17:18, 14)] # put columns in logical order
d2 <- d2[order]

# add studies back to data table
d2 <- rbind(d2, hampton[order]); rm(hampton)
d2 <- rbind(d2, johnson[order]); rm(johnson)
d2 <- rbind(d2, wolfe[order]); rm(wolfe)
d2 <- rbind(d2, reimers[order]); rm(reimers)
d2 <- rbind(d2, read[order]); rm(read)
d2 <- rbind(d2, li[order]); rm(li)
d2 <- rbind(d2, liu[order]); rm(liu)
d2 <- rbind(d2, green[order]); rm(green)
d2 <- rbind(d2, jimura[order]); rm(jimura)
d2 <- rbind(d2, whelan[order]); rm(whelan)

# Fix label in Garza
d2$Intervention[which(d2$Intervention == 'Old')] <- 'Older'

write.csv(d2, here::here('data', 'covidence.csv'), row.names = FALSE)

