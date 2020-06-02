# Convert to effect sizes
# 6.2.20 KLS

# load required packages
library(here)
library(tidyverse)
library(esc)

# load source functions

# set hard-coded variables
file <- 'cleaned.csv'

# load data
dt <- read.csv(here::here('data', file))

# make interaction term for study + conditions
dt$conditionID <- interaction(dt$Study.Identifier, dt$condition)

# Calculate effect sizes for continuous age designs
dc <- dt[which(dt$Design == 'continuous age'),] # pull out correlational studies
dc <- dc[!is.na(dc$correlation),] # remove incomplete studies

dc <- mutate(dc, 
       effect_size = esc_rpb(r = correlation, totaln = n, es.type = 'g')[1][[1]], 
       std_err = esc_rpb(r = correlation, totaln = n, es.type = 'g')[2][[1]], 
       var = esc_rpb(r = correlation, totaln = n, es.type = 'g')[3][[1]])
dc <- dc[c(1, 6, 8:12, 14, 23:26)]

## average effect size across multiple values within the same study 
average_within_study <- function(df, studyid) {
  x = df[which(df$Study.Identifier == studyid),] # pull out study of interest
  newmean <- cbind(x[1,1:9], t(colMeans(x[10:12]))) # average across estimates within same study
  dt <- df[-which(df$Study.Identifier == studyid),] # remove multiple estimates from df
  rbind(dt, newmean) # add new mean estimate to df
}

dc <- average_within_study(dc, 'Hampton 2018')
dc <-average_within_study(dc, 'Johnson 2015')
dc <- average_within_study(dc, 'Read 2004')

## remove term for study + conditions
dc <- dc[-9]

## effect per decade - what to do about variance??
#dc$adj_effect_size <- dc$effect_size * 10 

write.csv(dc, here::here('output', 'continuous_table.csv'), row.names = FALSE)
