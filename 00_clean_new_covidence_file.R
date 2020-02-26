# Sort data from covidence
# 2.26.20

# load required packages
library(here)
library(tidyverse)
library(magrittr)

# load source functions

# set hard-coded variables

# load data
dt <- read.csv(here::here('data', 'review_47118_extracted_data_csv_20200227082348.csv'))
               
# find columns that contain *only* of NA and eliminate
d1 <- dt[colSums(is.na(dt))/49 < 1]
#rm(dt)

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
  unite('correlation', colnames(d1)[grep('Discounting.Baseline.correlation', colnames(d1))], 
        sep='_', na.rm=TRUE) %>%
  unite('rsquare', c(Discounting.Baseline.rsquare1, Discounting.Baseline.rsquare2, 
                     Discounting.Baseline.rsquare3), sep='_', na.rm=TRUE) %>%
  unite('mean', colnames(d1)[c(grep('Discounting.Baseline.mean', 
                                    colnames(d1)), grep('Discounting.Baseline.Mean', colnames(d1)))], 
        sep = '_', na.rm=TRUE) %>%
  unite('sd', colnames(d1)[grep('Discounting.Baseline.SD', colnames(d1))], sep='_', na.rm=TRUE) %>%
  unite('tvalue', colnames(d1)[grep('Discounting.Baseline.t.value', colnames(d1))], 
                                    sep='_', na.rm=TRUE) 
  

colnames(d2)[which(colnames(d2) == 'Discounting.Baseline.SE')] <- 'se'
colnames(d2)[which(colnames(d2) == 'Discounting.Baseline.F.value')] <- 'Fvalue'
colnames(d2)[which(colnames(d2) == 'Discounting.Baseline.effectsize..d.')] <- 'effect_size_d'



  