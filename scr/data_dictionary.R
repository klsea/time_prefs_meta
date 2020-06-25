# Create data dictionary
# 6.25.20 KLS

# load required packages
library(here)
library(tidyverse)

# load source functions

# set hard-coded variables
file <- 'complete.csv'

# load data
dt <- read.csv(here::here('output', file))

# get colnames and data type for data dictionary
variable <- colnames(dt)
unit <- sapply(dt, class)

# create data dictionary
dd <- data.frame(variable, unit, matrix(ncol = 2, nrow = length(unit)))
colnames(dd) <- c('variable', 'unit', 'allowed_values', 'definition')

dd$allowed_values[1] <- 'Text'
dd$definition[1] <- 'Last name of first author & year. Identification variable for each study'

dd$unit[2] <- 'categorical'
dd$allowed_values[2] <- 'Younger, Older, Middle_1, Middle_2 or Age'
dd$definition[2] <- 'Conveys the source of the data. For extreme group designs, this labels the group name (e.g. Younger). 
For continous designs, the term "age" is used'

dd$allowed_values[3] <- 'Text'
dd$definition[3] <- "First and last name of the study's first author"

dd$allowed_values[4] <- 'Email address'
dd$definition[4] <- 'Email address of corresponding author'

dd$unit[5] <- 'time in years'
dd$allowed_values[5] <- 'YYYY'
dd$definition[5] <- 'Year of publication'

dd$unit[6] <- 'categorical'
dd$allowed_values[6] <- 'extreme group or continuous age'
dd$definition[6] <- 'Experimental design of the study'

dd$allowed_values[7] <- 'Text'
dd$definition[7] <- 'Please ignore this column. Incomplte notes about data extraction.'

dd$unit[6] <- 'categorical'
dd$allowed_values[8] <- 'real or hypothetical'
dd$definition[8] <- 'Type of incentive used for delay discounting in the study.'

dd$unit[9] <- 'categorical'
dd$allowed_values[9] <- 'hours, days, weeks, months or years'
dd$definition[9] <- 'Length of time delay used in temporal discounting task.'

dd$unit[10] <- 'categorical'
dd$allowed_values[10] <- 'proportion or parameter'
dd$definition[10] <- 'Description of how temporal discounting behavior was quantified. 
Proportion used for studies that reported age effect as proportion of smaller, sooner (or larger, later) choices made'

dd$allowed_values[11] <- 'Text'
dd$definition[11] <- 'For studies with multiple conditions, this is a label describing each condition.'

dd$allowed_values[12] <- '20 - 83'
dd$definition[12] <- 'Mean age of participants in that study or in that study group.'

dd$allowed_values[13] <- 'Text'
dd$definition[13] <- 'Age range of participants in that study or in that study group.'

dd$allowed_values[14] <- '2 - 21'
dd$definition[14] <- 'Standard deviation of the mean age of particiapnts in that study or in that study group.'

dd$allowed_values[15] <- '9 - 42,863'
dd$definition[15] <- 'Sample size of that study or that study group.'

dd$allowed_values[16] <- '0.0 - 1.0'
dd$definition[16] <- 'Correlation between age and measure of temporal discounting as reported in study. 
Some correlations may need to be reversed depending on how temporal discounting was measured in that study. 
See Seaman, et al 2020 for more details.'

dd$allowed_values[17] <- '-4.57 - 26.80'
dd$definition[17] <- 'Mean discounting for that study group. 
Some means may need to be reversed depending on how temporal discounting was measured in that study. 
See Seaman, et al 2020 for more details.'

dd$allowed_values[18] <- '0.03 - 8.13'
dd$definition[18] <- 'Standard deviation of the mean discounting for that study group.'

dd$allowed_values[19] <- '0.02 - 4.85'
dd$definition[19] <- 'Reported t-value for the group difference in discounting behavior'

dd$allowed_values[20] <- 'postiive whole number'
dd$definition[20] <- 'Degrees of freedom for t-test reported in t-value column.'

colnames(dd) <- c('Variable', 'Measurement Units', 'Allowed Values', 'Definition of Variable')

write.csv(dd, here::here('data', 'complete_data_dictionary.csv'))









