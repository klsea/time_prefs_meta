# Convert to effect sizes
# 5.13.20 KLS

# load required packages
library(here)
library(tidyverse)
library(metafor)

# load source functions

# set hard-coded variables
file <- 'covidence.csv'

# load data
dt <- read.csv(here::here('data', file))

# make interaction term for study + conditions
dt$conditionID <- interaction(dt$Study.Identifier, dt$condition)

# Calculate effect sizes for continuous age designs
dc <- dt[which(dt$Design == 'continuous age'),] # pull out correlational studies
dc <- dc[!is.na(dc$correlation),] # remove incomplete studies
dc <- escalc(measure ='ZCOR', ri=correlation, ni=n, data=dc) # yi = effect size, vi = sampling variances
dc <- dc[c(1, 6, 8:10, 14, 25:27)]

## average across multiple values within the same study
hampton <- dc[which(dc$Study.Identifier == 'Hampton 2018'),]
hampton <- cbind(hampton[1,1:7], t(colMeans(hampton[8:9])))
johnson <- dc[which(dc$Study.Identifier == 'Johnson 2015'),]
johnson <- cbind(johnson[1,1:7], t(colMeans(johnson[8:9])))

## remove studies with multiple condition rows and replace with study means
dc <- dc[-which(dc$Study.Identifier == 'Hampton 2018'),]
dc <- rbind(dc, hampton)
dc <- dc[-which(dc$Study.Identifier == 'Johnson 2015'),]
dc <- rbind(dc, johnson)
rm(hampton, johnson)

## remove term for study + conditions
dc <- dc[-7]

write.csv(dc, here::here('figs', 'continuous_table.csv'), row.names = FALSE)

# Calculate effect sizes for extreme group designs
dm <- dt[which(dt$Design == 'extreme group'),] # pull out means
dm <- dm[!is.na(dm$sd),]

### What to do about Garza 2016???
# Remove for now
dm <- dm[-c(grep('Garza', dm$Study.Identifier)),]

# Single measure studies

dm1 <- spread(dm[1:8, c(1, 2, 6, 8:10, 18, 25)], Intervention, mean)
colnames(dm1) <- c(colnames(dm1[1:6]), paste0('M_', colnames(dm1[,7:ncol(dm1)])))

dm2 <- spread(dm[1:8, c(1, 2, 6, 8:10, 19, 25)], Intervention, sd)
colnames(dm2) <- c(colnames(dm2[1:6 ]), paste0('SD_', colnames(dm2[,7:ncol(dm1)])))

dm3 <- spread(dm[1:8, c(1, 2, 6, 8:10, 14, 25)], Intervention, n)
colnames(dm3) <- c(colnames(dm3[1:6]), paste0('N_', colnames(dm3[,7:ncol(dm1)])))

dm4 <- merge(dm1, dm2,  by = c(colnames(dm1[1:6])))
dm5 <- merge(dm4, dm3, by = c(colnames(dm3[1:6])))

rm(dm1,dm2,dm3, dm4)

# multiple measure studies
# li & liu
dm0 <- dm[9:26,] #dm[13:30,]
dm0$conditionID <- interaction(dm0$Study.Identifier, dm0$condition)

dm1 <- spread(dm0[c(1, 2, 6, 8:10, 18, 25)], Intervention, mean)
colnames(dm1) <- c(colnames(dm1[1:6]), paste0('M_', colnames(dm1[,7:ncol(dm1)])))

dm2 <- spread(dm0[c(1, 2, 6, 8:10, 19, 25)], Intervention, sd)
colnames(dm2) <- c(colnames(dm2[1:6]), paste0('SD_', colnames(dm2[,7:ncol(dm2)])))

dm3 <- spread(dm0[c(1, 2, 6, 8:10, 14, 25)], Intervention, n)
colnames(dm3) <- c(colnames(dm3[1:6]), paste0('N_', colnames(dm3[,7:ncol(dm3)])))

dm4 <- merge(dm1, dm2,  by = c(colnames(dm1[1:6])))
dm6 <- merge(dm4, dm3, by = c(colnames(dm3[1:6])))

rm(dm0, dm1, dm2, dm3, dm4)

# together
dm7 <- rbind(dm5, dm6)
dm <- escalc(measure = 'SMD', n1i = N_Older, n2i = N_Younger, m1i = M_Older, m2i = M_Younger, sd1i = SD_Older, sd2i = SD_Younger, data=dm7)
rm(dm5, dm6, dm7)

## average across multiple values within the same study
li <- dm[which(dm$Study.Identifier == 'Li 2013'),]
li <- cbind(li[1,1:12], t(colMeans(li[13:14])))
liu <- dm[which(dm$Study.Identifier == 'Liu 2016'),]
liu <- cbind(liu[1,1:12], t(colMeans(liu[13:14])))

## remove studies with multiple condition rows and replace with study means
dm <- dm[-which(dm$Study.Identifier == 'Li 2013'),]
dm <- rbind(dm, li)
dm <- dm[-which(dm$Study.Identifier == 'Liu 2016'),]
dm <- rbind(dm, liu)
rm(li, liu)

## calculate sample size
dm$n = dm$N_Older + dm$N_Younger

write.csv(dm[-c(6:10, 15)], here::here('figs', 'extreme_table.csv'), row.names = FALSE)

## remove term for study + conditions and other intermediate terms
dm <- dm[-c(6:12)]
dm <- dm[c(1:5, 8, 6:7)]

# Concatenate dc and dm tables
es <- rbind(dc, dm)

# Save as data file
write.csv(es, here::here('data', 'effect_sizes.csv'), row.names = FALSE)
