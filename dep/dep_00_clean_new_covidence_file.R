# Sort data from covidence
# 2.26.20 updated 3.19.20

# load required packages
library(here)
library(tidyverse)
library(magrittr)
library(janitor)
library(plyr)

# load source functions

# set hard-coded variables

# load data
dt <- read.csv(here::here('data', 'review_47118_extracted_data_csv_20200227082348.csv'))
               
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

# add condition column
d2$Condition <- NULL

# ---------------
# Correlations
# ---------------
colnames(d2)[which(colnames(d2) == 'Discounting.Baseline.correlation')] <- 'correlation'

# Hampton 2018
dt <- d2[which(d2$Study.Identifier == 'Hampton 2018'),] # isolate Hampton data
dt <- dt[,colSums(is.na(dt)) == 0] # remove NA columns
cond <- c(1, 7, 30, 180, 365)
cols <- paste0('Discounting.Baseline.correlation', cond)
dt <- gather(dt, condition, correlation, cols)
dt$condition <- gsub('Discounting.Baseline.correlation', '', dt$condition)
dt$condition <- paste0(dt$condition, ' days')
dt['se'] <- NA; dt['sd'] <- NA; dt['Fvalue'] <- NA; dt['mean'] <- NA; 
dt['effect_size_d'] <- NA; dt['rsquare'] <- NA
hampton <- dt
rm(dt,cond,cols)
d2 <- d2[-which(d2$Study.Identifier == 'Hampton 2018'),] # Remove Hampton from d2 dataframe; will put back in later

# Johnson 2015
dt <- d2[which(d2$Study.Identifier == 'Johnson 2015'),] # isolate Johnson data
dt <- dt[,colSums(is.na(dt)) == 0] # remove NA columns
cond <- c(1, 2)
cols <- paste0('Discounting.Baseline.correlation.', cond)
dt <- gather(dt, condition, correlation, cols)
dt$condition <- gsub('Discounting.Baseline.correlation.', '', dt$condition)
dt$condition <- 10^as.numeric(dt$condition)
dt$condition <- paste0('$', dt$condition)
dt['se'] <- NA; dt['sd'] <- NA; dt['Fvalue'] <- NA; dt['mean'] <- NA; 
dt['effect_size_d'] <- NA; dt['rsquare'] <- NA
johnson <- dt
rm(dt,cond,cols)
d2 <- d2[-which(d2$Study.Identifier == 'Johnson 2015'),]

# Wolfe 2017
dt <- d2[which(d2$Study.Identifier == 'Wolfe 2017'),] # isolate Wolfe data
dt <- dt[,colSums(is.na(dt)) == 0] # remove NA columns
colnames(dt)[which(colnames(dt) == 'Discounting.Baseline.correlation.3')] <- 'correlation'
dt['se'] <- NA; dt['sd'] <- NA; dt['Fvalue'] <- NA; dt['mean'] <- NA; 
dt['effect_size_d'] <- NA; dt['condition'] <- NA; dt['rsquare'] <- NA
wolfe <- dt
rm(dt)
d2 <- d2[-which(d2$Study.Identifier == 'Wolfe 2017'),]

# remove unneccesary correlation columns
d2 <- d2[-grep('Discounting.Baseline.correlation', colnames(d2))]

# --------------
# R-Square
# ---------------
# Read 2004
dt <- d2[which(d2$Study.Identifier == 'Read 2004'),] # isolate Read data
dt <- dt[,colSums(is.na(dt)) == 0] # remove NA columns
cond <- c(1, 2, 3)
cols <- paste0('Discounting.Baseline.rsquare', cond)
dt <- gather(dt, condition, rsquare, cols)
dt$condition <- gsub('Discounting.Baseline.rsquare', '', dt$condition)
dt$condition <- c('0-3 year delays', '0-10 year delays', '7-10 year delays')
dt['se'] <- NA; dt['sd'] <- NA; dt['Fvalue'] <- NA; dt['mean'] <- NA; 
dt['effect_size_d'] <- NA; dt['condition'] <- NA; dt['correlation'] <- NA
read <- dt
rm(dt,cond,cols)
d2 <- d2[-which(d2$Study.Identifier == 'Read 2004'),]

# remove unneccesary rsquare columns
d2 <- d2[-grep('Discounting.Baseline.rsquare', colnames(d2))]
d2$rsquare <- NA

# -------------------
# Group Means and SDs
# -------------------
colnames(d2)[which(colnames(d2) == 'Discounting.Baseline.mean')] <- 'mean'
colnames(d2)[which(colnames(d2) == 'Discounting.Baseline.SD.1')] <- 'sd'

# Li 2013
dt <- d2[which(d2$Study.Identifier == 'Li 2013'),] # isolate Li data
dt <- dt[,colSums(is.na(dt)) == 0] # remove NA columns
cond <- c(1, 2.1, 3, 4, 5)
cols <- paste0('Discounting.Baseline.Mean', cond)
means <- gather(dt, condition, mean, cols)
means <- means[-grep('Discounting.Baseline.SD', colnames(means))]
means$condition <- gsub('Discounting.Baseline.Mean', '', means$condition)
means$condition <- revalue(means$condition, c('1' = '$60, 4 mo', '2.1' = '$75, 3 mo', 
                                              '3' = '$55, 3 mo', '4' = '$115, 3 mo', 
                                              '5' = '$100, 12 mo'))
cond <- c(1, 2.1, 3.1, 4, 5)
cols <- paste0('Discounting.Baseline.SD', cond)
sds <- gather(dt, condition, sd, cols)
sds <- sds[-grep('Discounting.Baseline.Mean', colnames(sds))]
sds$condition <- gsub('Discounting.Baseline.SD', '', sds$condition)
sds$condition <- revalue(sds$condition, c('1' = '$60, 4 mo', '2.1' = '$75, 3 mo', 
                                              '3.1' = '$55, 3 mo', '4' = '$115, 3 mo', 
                                              '5' = '$100, 12 mo'))
dt <- merge(means, sds)
dt['se'] <- NA; dt['Fvalue'] <- NA; dt['effect_size_d'] <- NA; dt['condition'] <- NA; 
dt['correlation'] <- NA; dt['rsquare'] <- NA
li <- dt
rm(dt,cond,cols, means, sds)
d2 <- d2[-which(d2$Study.Identifier == 'Li 2013'),]

# Liu 2016
dt <- d2[which(d2$Study.Identifier == 'Liu 2016'),] # isolate Liu data
dt <- dt[,colSums(is.na(dt)) == 0] # remove NA columns
cond <- c(1.2, 2.2, 3.1, 4.1)
cols <- paste0('Discounting.Baseline.Mean', cond)
means <- gather(dt, condition, mean, cols)
means <- means[-grep('Discounting.Baseline.SD', colnames(means))]
means$condition <- gsub('Discounting.Baseline.Mean', '', means$condition)
means$condition <- revalue(means$condition, c('1.2' = 'Ln(k) small', '2.2' = 'Ln(k) medium', 
                                              '3.1' = 'Ln(k) large', '4.1' = 'Ln(k) mean'))
cond <- c(1.1, 2.2, 3, 4.1)
cols <- paste0('Discounting.Baseline.SD', cond)
sds <- gather(dt, condition, sd, cols)
sds <- sds[-grep('Discounting.Baseline.Mean', colnames(sds))]
sds$condition <- gsub('Discounting.Baseline.SD', '', sds$condition)
sds$condition <- revalue(sds$condition, c('1.1' = 'Ln(k) small', '2.2' = 'Ln(k) medium', 
                                         '3' = 'Ln(k) large', '4.1' = 'Ln(k) mean'))
dt <- merge(means, sds)
dt['se'] <- NA; dt['Fvalue'] <- NA; dt['effect_size_d'] <- NA; dt['condition'] <- NA; 
dt['correlation'] <- NA; dt['rsquare'] <- NA
liu <- dt
rm(dt,cond,cols, means, sds)
d2 <- d2[-which(d2$Study.Identifier == 'Liu 2016'),]

# Green 1994
dt <- d2[which(d2$Study.Identifier == 'GREEN 1994'),] # isolate Green data
dt <- dt[,colSums(is.na(dt)) == 0] # remove NA columns
cond <- c('1.000.', '10.000.')
cols <- paste0('Discounting.Baseline.Mean..', cond)
dt <- gather(dt, condition, mean, cols)
dt$condition <- gsub('Discounting.Baseline.Mean..', '', dt$condition)
dt$condition <- revalue(dt$condition, c('1.000.' = '$1,000', '10.000.' = '$10,000'))
dt['se'] <- NA; dt['effect_size_d'] <- NA; dt['condition'] <- NA; 
dt['correlation'] <- NA; dt['rsquare'] <- NA; dt['sd'] <- NA
green <- dt
rm(dt,cond,cols)
d2 <- d2[-which(d2$Study.Identifier == 'GREEN 1994'),]

# Jimura 2011
dt <- d2[which(d2$Study.Identifier == 'Jimura 2011'),] # isolate Jimura data
dt <- dt[,colSums(is.na(dt)) == 0] # remove NA columns
colnames(dt)[which(colnames(dt) == 'Discounting.Baseline.Mean')] <- 'mean'
dt['se'] <- NA; dt['Fvalue'] <- NA; dt['effect_size_d'] <- NA; dt['condition'] <- NA; 
dt['correlation'] <- NA; dt['rsquare'] <- NA; dt['sd'] <- NA
jimura <- dt
rm(dt)
d2 <- d2[-which(d2$Study.Identifier == 'Jimura 2011'),]

# Whelan 2009
dt <- d2[which(d2$Study.Identifier == 'Whelan 2009'),] # isolate Whelan data
dt <- dt[,colSums(is.na(dt)) == 0] # remove NA columns
cond <- c('1.1', '2')
cols <- paste0('Discounting.Baseline.Mean', cond)
dt <- gather(dt, condition, mean, cols)
dt$condition <- gsub('Discounting.Baseline.Mean', '', dt$condition)
dt$condition <- revalue(dt$condition, c('1.1' = '£100', '2' = '£1,000'))
dt['se'] <- NA; dt['Fvalue'] <- NA; dt['effect_size_d'] <- NA; dt['condition'] <- NA; 
dt['correlation'] <- NA; dt['rsquare'] <- NA; dt['sd'] <- NA
whelan <- dt
rm(dt,cond,cols)
d2 <- d2[-which(d2$Study.Identifier == 'Whelan 2009'),]

# remove unneccesary mean and sd columns
d2 <- d2[-grep('Discounting.Baseline.Mean', colnames(d2))]
d2 <- d2[-grep('Discounting.Baseline.SD', colnames(d2))]

# ------------------------
# Put it all back together
# ------------------------
d2$condition <- NA
order <- colnames(d2)
order <- order[c(1:13, 15, 24, 16, 19:23, 17:18, 14)]
d2 <- d2[order]

# add studies back to data table
d2 <- rbind(d2, hampton[order]); rm(hampton)
d2 <- rbind(d2, johnson[order]); rm(johnson)
d2 <- rbind(d2, wolfe[order]); rm(wolfe)
d2 <- rbind(d2, read[order]); rm(read)
d2 <- rbind(d2, li[order]); rm(li)
d2 <- rbind(d2, liu[order]); rm(liu)
d2 <- rbind(d2, green[order]); rm(green)
d2 <- rbind(d2, jimura[order]); rm(jimura)
d2 <- rbind(d2, whelan[order]); rm(whelan)
