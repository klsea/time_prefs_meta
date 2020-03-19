# Sort data from covidence by study
# 3.18.20

# load required packages
library(here)
library(tidyverse)
library(janitor)


# load source functions

# set hard-coded variables

# load data
dt <- read.csv(here::here('data', 'review_47118_extracted_data_csv_20200227082348.csv'))

## clean up original data frame
# find columns that contain *only* NA and eliminate
dt <- dt[colSums(is.na(dt))/49 < 1]
# put in alpha order
dt <- dt[order(dt$Study.Identifier),]

# create new data frame

# Bauer 2010
bauer <- dt[which(dt$Study.Identifier == 'Bauer 2010'),]
bauer <- (bauer) %>% remove_empty('cols')
colnames(bauer) <- gsub('Baseline.', '', colnames(bauer))
bauer <- bauer[-c(grep('Range', colnames(bauer))[-1], grep('Correct', colnames(bauer)))]
colnames(bauer)[which(colnames(bauer) == 'Discounting.mean')] <- 'Discounting.Mean'
colnames(bauer)[which(colnames(bauer) == 'Discounting.N.11')] <- 'Discounting.N'
colnames(bauer)[which(colnames(bauer) == 'Discounting.SD.1')] <- 'Discounting.SD'

# Bickel 2014 
bickel <- dt[which(dt$Study.Identifier == 'Bauer 2010'),]
