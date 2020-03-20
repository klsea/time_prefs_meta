# Sort data from covidence
# 9.5.19 KLS & 10.10.19

# load required packages
#library(gdata)
library(readxl)
library(here)

# load source functions
source(here('scr', 'extract_continuous.R'))

# set hard-coded variables
#filename = 'review_47118_included_xlsx_20190906031623.xlsx'
#filename = 'review_47118_included_xlsx_20191011045854.xlsx'
filename = 'review_47118_extracted_data_xlsx_20200319142630.xlsx'

# load data
names <- excel_sheets(here::here('data', filename))
names <- names[2:length(names)]
n_studies<- length(names)
dt1<- data.frame(matrix(ncol=3,nrow=n_studies))
dt1[1] <- names

# extract the type of study
for (i in 1:n_studies+1) {
  print(i)
  dt <- read_excel(here('data', filename), sheet = i)[1:40,]
  dt1[i-1,2] <- dt[which(dt$Agreed == "Design"),][,2]
  dt1[i-1,3] <- i
}
rm(dt)
colnames(dt1) <- c('Study', 'Design', 'Sheet')

# make design names consistent
dt1$Design <- factor(dt1$Design)
dt1$Design[which(dt1$Design == 'Continuous Age')] <- 'continuous age'
dt1$Design[which(dt1$Design == 'Extreme Group')] <- 'extreme group'
dt1$Design <- factor(dt1$Design)

# extract continuous age designs from covidence data
dt2 <- extract_continuous(filename, dt1)
write.csv(dt2, 'output/covidence_continuous.csv', row.names = FALSE)

# create a data table for extreme group studies



