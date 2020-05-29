# Convert to effect sizes
# 5.13.20 KLS updated 5.27.19

# load required packages
library(here)
library(tidyverse)
library(metafor)
library(esc)

# load source functions

# set hard-coded variables
file <- 'cleaned.csv'

# load data
dt <- read.csv(here::here('data', file))

# make interaction term for study + conditions
dt$conditionID <- interaction(dt$Study.Identifier, dt$condition)

# Calculate effect sizes for extreme group designs
dm <- dt[which(dt$Design == 'extreme group'),] # pull out means
ds <- dm[is.na(dm$sd),]
dm <- dm[!is.na(dm$sd),]

### What to do about Garza 2016???
# Remove for now
dm <- dm[-c(grep('Garza', dm$Study.Identifier)),]

# Single measure studies

dm1 <- spread(dm[1:8, c(1, 2, 6, 8:10, 18, 23)], Intervention, mean)
colnames(dm1) <- c(colnames(dm1[1:6]), paste0('M_', colnames(dm1[,7:ncol(dm1)])))

dm2 <- spread(dm[1:8, c(1, 2, 6, 8:10, 19, 23)], Intervention, sd)
colnames(dm2) <- c(colnames(dm2[1:6 ]), paste0('SD_', colnames(dm2[,7:ncol(dm2)])))

dm3 <- spread(dm[1:8, c(1, 2, 6, 8:10, 14, 23)], Intervention, n)
colnames(dm3) <- c(colnames(dm3[1:6]), paste0('N_', colnames(dm3[,7:ncol(dm3)])))

dm4 <- spread(dm[1:8, c(1, 2, 6, 8:10, 11, 23)], Intervention, age_mean)
colnames(dm4) <- c(colnames(dm4[1:6]), paste0('Age_Mean_', colnames(dm4[,7:ncol(dm4)])))

dm5 <- spread(dm[1:8, c(1, 2, 6, 8:10, 12, 23)], Intervention, age_range)
colnames(dm5) <- c(colnames(dm5[1:6]), paste0('Age_Range_', colnames(dm5[,7:ncol(dm5)])))

dm6 <- merge(dm1, dm2,  by = c(colnames(dm1[1:6])))
dm7 <- merge(dm6, dm3, by = c(colnames(dm3[1:6])))
dm8 <- merge(dm7, dm4, by = c(colnames(dm4[1:6])))
sms <- merge(dm8, dm5, by = c(colnames(dm5[1:6])))

rm(dm1,dm2,dm3, dm4, dm5, dm6, dm7, dm8)

# multiple measure studies
# li & liu
start <- grep('Li', dm$Study.Identifier)[1]
end <- tail(grep('Li', dm$Study.Identifier), n =1)
dm0 <- dm[start:end,]
dm0$conditionID <- interaction(dm0$Study.Identifier, dm0$condition)

dm1 <- spread(dm0[c(1, 2, 6, 8:10, 18, 23)], Intervention, mean)
colnames(dm1) <- c(colnames(dm1[1:6]), paste0('M_', colnames(dm1[,7:ncol(dm1)])))

dm2 <- spread(dm0[c(1, 2, 6, 8:10, 19, 23)], Intervention, sd)
colnames(dm2) <- c(colnames(dm2[1:6]), paste0('SD_', colnames(dm2[,7:ncol(dm2)])))

dm3 <- spread(dm0[c(1, 2, 6, 8:10, 14, 23)], Intervention, n)
colnames(dm3) <- c(colnames(dm3[1:6]), paste0('N_', colnames(dm3[,7:ncol(dm3)])))

dm4 <- spread(dm0[c(1, 2, 6, 8:10, 11, 23)], Intervention, age_mean)
colnames(dm4) <- c(colnames(dm4[1:6]), paste0('Age_Mean_', colnames(dm4[,7:ncol(dm4)])))

dm5 <- spread(dm0[c(1, 2, 6, 8:10, 12, 23)], Intervention, age_range)
colnames(dm5) <- c(colnames(dm5[1:6]), paste0('Age_Range_', colnames(dm5[,7:ncol(dm5)])))

dm6 <- merge(dm1, dm2,  by = c(colnames(dm1[1:6])))
dm7 <- merge(dm6, dm3, by = c(colnames(dm3[1:6])))
dm8 <- merge(dm7, dm4, by = c(colnames(dm4[1:6])))
mms <- merge(dm8, dm5, by = c(colnames(dm5[1:6])))

rm(dm0, dm1, dm2, dm3, dm4, dm5, dm6, dm7, dm8)

# together
dm <- rbind(sms, mms)
dm <- escalc(measure = 'SMD', n1i = N_Older, n2i = N_Younger, m1i = M_Older, m2i = M_Younger, sd1i = SD_Older, sd2i = SD_Younger, data=dm)
rm(sms, mms)

## average across multiple values within the same study
li <- dm[which(dm$Study.Identifier == 'Li 2013'),]
li <- cbind(li[1,1:16], t(colMeans(li[17:18])))
liu <- dm[which(dm$Study.Identifier == 'Liu 2016'),]
liu <- cbind(liu[1,1:16], t(colMeans(liu[17:18])))

## remove studies with multiple condition rows and replace with study means
dm <- dm[-which(dm$Study.Identifier == 'Li 2013'),]
dm <- rbind(dm, li)
dm <- dm[-which(dm$Study.Identifier == 'Liu 2016'),]
dm <- rbind(dm, liu)
rm(li, liu)

dm$M_Older <- NULL
dm$M_Younger <- NULL
dm$SD_Older <- NULL
dm$SD_Younger <- NULL
dm$conditionID <- NULL
order <- colnames(dm)

# calculate effect size from t vals
dt0 <- ds[!is.na(ds$tvalue),]
dt1 <- spread(dt0[c(1:2, 6, 8:10, 14, 20)], Intervention, n)
dt2 <- matrix(nrow = nrow(dt1), ncol = 3)
for (r in 1:nrow(dt1)) {
  dt2[r,1] = as.character(dt1$Study.Identifier[r]) 
  dt2[r,2] = esc_t(t = dt1$tvalue[r], grp1n = dt1$Older[r], grp2n = dt1$Younger[r], es.type = "d")[1][[1]]
  dt2[r,3] = esc_t(t = dt1$tvalue[r], grp1n = dt1$Older[r], grp2n = dt1$Younger[r], es.type = "d")[3][[1]]
}
colnames(dt2) <- c('Study.Identifier', 'yi', 'vi')
dt1 <- merge(dt1, dt2, by = 'Study.Identifier')
colnames(dt1) <- c(colnames(dt1[1:6]), paste0('N_', colnames(dt1[, 7:8])), colnames(dt1[9:10]))

dt3 <- spread(dt0[c(1:2, 6, 8:10, 11, 20)], Intervention, age_mean)
colnames(dt3) <- c(colnames(dt3[1:6]), paste0('Age_Mean_', colnames(dt3[, 7:ncol(dt3)])))

dt4 <- spread(dt0[c(1:2, 6, 8:10, 12, 20)], Intervention, age_range)
colnames(dt4) <- c(colnames(dt4[1:6]), paste0('Age_Range_', colnames(dt4[, 7:ncol(dt4)])))

est <- merge(dt1, dt3)
est <- merge(est, dt4) # effect size from t values table
rm(dt0, dt1, dt2, dt3, dt4)
est$tvalue <- NULL
est <- est[order]
est$yi <- as.numeric(as.character(est$yi))
est$vi <- as.numeric(as.character(est$vi))

# calculate effect size from f vals
dt3 <- ds[!is.na(ds$Fvalue),]
dt3 <- spread(dt3[c(1:2, 14, 21, 23)], Intervention, n)
dt4 <- matrix(nrow = nrow(dt3), ncol = 3)
for (r in 1:nrow(dt3)) {
  dt4[r,1] = as.character(dt3$conditionID[r])
  dt4[r,2] = esc_f(f = dt3$Fvalue[r], grp1n = dt3$Older[r], grp2n = dt3$Younger[r], es.type = "d")[1][[1]]
  dt4[r,3] = esc_f(f = dt3$Fvalue[r], grp1n = dt3$Older[r], grp2n = dt3$Younger[r], es.type = "d")[3][[1]]
}
colnames(dt4) <- c('conditionID', 'yi', 'vi')
dt3 <- merge(dt3, dt4, by = 'conditionID')
dt3$yi <- as.numeric(as.character(dt3$yi))
dt3$vi <- as.numeric(as.character(dt3$vi))
rm(dt4)

## average across multiple values within the same study
dt3 <- cbind(dt3[1,2:5], t(colMeans(dt3[6:7])))

# merge and save table
ddm <- bind_rows(dm, est)

#write.csv(dm[-c(6:10)], here::here('figs', 'extreme_table.csv'), row.names = FALSE)


## Reconsider below

## remove term for study + conditions and other intermediate terms
dm <- dm[-c(6:12)]
dm <- dm[c(1:5, 8, 6:7)]

# Concatenate dc and dm tables
es <- rbind(dc, dm)

# Save as data file
write.csv(es, here::here('data', 'effect_sizes.csv'), row.names = FALSE)