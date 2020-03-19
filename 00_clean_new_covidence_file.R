# Sort data from covidence
# 2.26.20 updated 3.19.20

# load required packages
library(here)
library(tidyverse)
library(magrittr)
library(janitor)

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
colnames(d2)[which(colnames(d2) == 'Discounting.Baseline.correlation')] <- 'Correlation'

# Hampton 2018
dt <- d2[which(d2$Study.Identifier == 'Hampton 2018'),] # isolate Hampton data
dt <- dt[,colSums(is.na(dt)) == 0] # remove NA columns
cond <- c(1, 7, 30, 180, 365)
cols <- paste0('Discounting.Baseline.correlation', cond)
dt <- gather(dt, condition, correlation, cols)
hampton <- dt
rm(dt,cond,cols)

# Johnson 2015
dt <- d2[which(d2$Study.Identifier == 'Johnson 2015'),] # isolate Hampton data
dt <- dt[,colSums(is.na(dt)) == 0] # remove NA columns
cond <- c(1, 2)
cols <- paste0('Discounting.Baseline.correlation.', cond)
dt <- gather(dt, condition, correlation, cols)
johnson <- dt
rm(dt,cond,cols)

# Wolfe 2017
dt <- d2[which(d2$Study.Identifier == 'Wolfe 2017'),] # isolate Hampton data
dt <- dt[,colSums(is.na(dt)) == 0] # remove NA columns
colnames(dt)[which(colnames(dt) == 'Discounting.Baseline.correlation.3')] <- 'correlation'
wolfe <- dt
rm(dt)

# remove unneccesary correlation columns
d2 <- d2[-grep('Discounting.Baseline.correlation', colnames(d2))]

# --------------
# R-Square
# ---------------


# unite('correlation', colnames(d1)[grep('Discounting.Baseline.correlation', colnames(d1))], 
#       sep='_', na.rm=TRUE) %>%
# unite('rsquare', c(Discounting.Baseline.rsquare1, Discounting.Baseline.rsquare2, 
#                    Discounting.Baseline.rsquare3), sep='_', na.rm=TRUE) %>%
# unite('mean', colnames(d1)[c(grep('Discounting.Baseline.mean', 
#                                   colnames(d1)), grep('Discounting.Baseline.Mean', colnames(d1)))], 
#       sep = '_', na.rm=TRUE) %>%
# unite('sd', colnames(d1)[grep('Discounting.Baseline.SD', colnames(d1))], sep='_', na.rm=TRUE) %>%

  