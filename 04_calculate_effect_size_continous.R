# Convert to effect sizes
# 5.13.20 KLS

# load required packages
library(here)
library(tidyverse)
library(esc)

# load source functions
source(here::here('scr', 'reverse_es.R'))

# set hard-coded variables
file <- 'complete.csv'

# load data
dt <- read.csv(here::here('output', file))

# calc effect size for continous design ####
# make interaction term for study + conditions
dt$conditionID <- interaction(dt$Study.Identifier, dt$condition)

# Calculate effect sizes for continuous age designs
dc <- dt[which(dt$Design == 'continuous age'),] # pull out correlational studies
dc <- dc[!is.na(dc$correlation),] # remove incomplete studies

dc <- mutate(dc, 
             effect_size = esc_rpb(r = correlation, totaln = n, es.type = 'g')[1][[1]], 
             std_err = esc_rpb(r = correlation, totaln = n, es.type = 'g')[2][[1]], 
             var =esc_rpb(r = correlation, totaln = n, es.type = 'g')[3][[1]]
)


# average effect size across multiple values within the same study ####
average_within_study <- function(df, studyid) {
  x = df[which(df$Study.Identifier == studyid),] # pull out study of interest
  newmean <- cbind(x[1,1:23], t(colMeans(x[24:26]))) # average across estimates within same study
  dt <- df[-which(df$Study.Identifier == studyid),] # remove multiple estimates from df
  rbind(dt, newmean) # add new mean estimate to df
}

dc <- average_within_study(dc, 'Hampton 2018')
dc <- average_within_study(dc, 'Johnson 2015')
dc <- average_within_study(dc, 'Mahalingam 2018 Sample 3')
dc <- average_within_study(dc, 'Mahalingam 2018 Sample 4')
dc <- average_within_study(dc, 'Mahalingam 2018 Sample 5')
dc <- average_within_study(dc, 'Mahalingam 2018 Sample 6')

# remove unnecessary columns
dc <- dc[c(1, 6, 8:12, 14, 24:26)]

## effect per decade ###
dc$adj_effect_size <- dc$effect_size * 10 

# Reversals ####
dc <- reverse_es(dc, 'Löckenhoff 2011')
dc <- reverse_es(dc, 'Hampton 2018')
dc <- reverse_es(dc, 'Wolfe 2017')

write.csv(dc, here::here('output', 'continuous_table.csv'), row.names = FALSE)
rm(dc, dt, file, average_within_study, reverse_es)


