# Convert to effect sizes from t or F vals
# 5.13.20 KLS updated 5.27.19

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

# Calculate effect sizes for extreme group designs
dm <- dt[which(dt$Design == 'extreme group'),] # pull out means
ds <- dm[is.na(dm$sd),] # pull out no sd files

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
est$yi <- as.numeric(as.character(est$yi))
est$vi <- as.numeric(as.character(est$vi))

order <- colnames(est)
order <- order[c(1:7, 10:13, 8:9)]
est <- est[order]

# calculate effect size from f vals
dt0 <- ds[!is.na(ds$Fvalue),]
dt1 <- spread(dt0[c(1:2,  6, 8:10, 14, 21, 23)], Intervention, n)
dt2 <- matrix(nrow = nrow(dt1), ncol = 3)

for (r in 1:nrow(dt1)) {
  dt2[r,1] = as.character(dt1$conditionID[r])
  dt2[r,2] = esc_f(f = dt1$Fvalue[r], grp1n = dt1$Older[r], grp2n = dt1$Younger[r], es.type = "d")[1][[1]]
  dt2[r,3] = esc_f(f = dt1$Fvalue[r], grp1n = dt1$Older[r], grp2n = dt1$Younger[r], es.type = "d")[3][[1]]
}
colnames(dt2) <- c('conditionID', 'yi', 'vi')
dt1 <- merge(dt1, dt2, by = 'conditionID')
colnames(dt1) <- c(colnames(dt1[1:7]), paste0('N_', colnames(dt1[, 8:9])), colnames(dt1[10:11]))

dt3 <- spread(dt0[c(1:2, 6, 8:10, 11, 23)], Intervention, age_mean)
colnames(dt3) <- c(colnames(dt3[1:6]), paste0('Age_Mean_', colnames(dt3[, 7:ncol(dt3)])))

dt4 <- spread(dt0[c(1:2, 6, 8:10, 12, 23)], Intervention, age_range)
colnames(dt4) <- c(colnames(dt4[1:6]), paste0('Age_Range_', colnames(dt4[, 7:ncol(dt4)])))

esf <- merge(dt1, dt3)
esf <- merge(esf, dt4)
rm(dt0, dt1, dt2, dt3, dt4)

## average across multiple values within the same study
esf$yi <- as.numeric(as.character(esf$yi))
esf$vi <- as.numeric(as.character(esf$vi))
esf <- cbind(esf[1, c(1:9, 12:15)], t(colMeans(esf[10:11])))

esf$conditionID <- NULL
esf$Fvalue <- NULL

ds <- rbind(esf, est)

  