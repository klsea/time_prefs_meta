# Calculate missing values 
# 3.20.20 

# load required packages
library(here)

# load source functions
#source(here::here('scr', 'conversion_functions.R'))

# set hard-coded variables
file <- 'cleaned.csv'

# load data
dt <- read.csv(here::here('data', file))

# pull in values for Sparrow 2018 from plot digitizer and name Sparrow 2018a
# used https://automeris.io/WebPlotDigitizer/
sparrow <- read.csv(here::here('data', 'sparrow2018plotdigit.csv'), header = FALSE)
dt$Study.Identifier <- as.character(dt$Study.Identifier)
dt[which(dt$correct_data_extracted == 'No mean or sd but we can extract from barplot'),
   which(colnames(dt) == 'Study.Identifier')] <- 'Sparrow 2018a' 
dt$Study.Identifier <- factor(dt$Study.Identifier)  
dt[which(dt$Study.Identifier == 'Sparrow 2018a' & dt$Intervention == 'Younger'), which(colnames(dt) == 'mean')] <- sparrow[2,3]
dt[which(dt$Study.Identifier == 'Sparrow 2018a' & dt$Intervention == 'Younger'), which(colnames(dt) == 'se')] <- sparrow[2,3] - sparrow[1,3]
dt[which(dt$Study.Identifier == 'Sparrow 2018a' & dt$Intervention == 'Older'), which(colnames(dt) == 'mean')] <- sparrow[5,3]
dt[which(dt$Study.Identifier == 'Sparrow 2018a' & dt$Intervention == 'Older'), which(colnames(dt) == 'se')] <- sparrow[5,3] - sparrow[4,3]
rm(sparrow)

# calculate means and sds from Whelan 2009
whelan <- read.csv(here::here('data', 'Whelan2009data.csv'))
mean <- colMeans(whelan, na.rm = TRUE)
sd <- apply(whelan, 2, sd, na.rm =TRUE)

dt[which(dt$condition == '£100' & dt$Intervention == 'Younger'), which(colnames(dt) == 'mean')] <- mean[3][[1]]
dt[which(dt$condition == '£100' & dt$Intervention == 'Younger'), which(colnames(dt) == 'sd')] <- sd[3][[1]]
dt[which(dt$condition == '£100' & dt$Intervention == 'Older'), which(colnames(dt) == 'mean')] <- mean[5][[1]]
dt[which(dt$condition == '£100' & dt$Intervention == 'Older'), which(colnames(dt) == 'sd')] <- sd[5][[1]]
dt[which(dt$condition == '£1,000' & dt$Intervention == 'Younger'), which(colnames(dt) == 'mean')] <- mean[4][[1]]
dt[which(dt$condition == '£1,000' & dt$Intervention == 'Younger'), which(colnames(dt) == 'sd')] <- sd[4][[1]]
dt[which(dt$condition == '£1,000' & dt$Intervention == 'Older'), which(colnames(dt) == 'mean')] <- mean[6][[1]]
dt[which(dt$condition == '£1,000' & dt$Intervention == 'Older'), which(colnames(dt) == 'sd')] <- sd[6][[1]]
rm(whelan, mean, sd)


# convert rsquare to r
dt[which(dt$rsquare != 'NA'),]$correlation <- sqrt(dt[which(dt$rsquare != 'NA'),]$rsquare)
dt$rsquare <- NULL

# convert se to sd
dt[which(dt$se != 'NA'),]$sd <- dt[which(dt$se != 'NA'),]$se * sqrt(dt[which(dt$se != 'NA'),]$n)
dt$se <- NULL

# write data file
write.csv(dt, here::here('data', 'cleaned.csv'), row.names = FALSE)

