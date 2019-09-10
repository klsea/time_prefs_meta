extract_continuous <- function(fn, dt) {
  # fn = the filename of the excel file extracted from covidence
  # dt = the data table listing (1) study name (2) design and (3) corresponding excel 
  # worksheet in the covidence spreadsheet
  
  dt2 <- dt[which(dt$Design == 'continuous age'),]
  for (j in 1:nrow(dt2)) {
    sheet = dt2$Sheet[j]
    dt <- read_excel(here('data', fn), sheet = sheet)
    start <- grep('Outcomes',dt$Agreed)[1]
    df <- dt[(start):(start+4),]
    size = ncol(df)
    
    if (size == 4 && is.na(df[5,4])) {
      #print('normal')
      df1 <- df[4:5,2:3]
      dt2[j,4] <- df1[2,1]
      dt2[j,5] <- df1[2,2] 
    } else {
      print('find average')
      df1 <- df[4:5,]
      dt2[j,4] <- mean(as.numeric(df1[2,2:(size-1)]))
      dt2[j,5] <- df1[2,size]
    }
  }
  rm(dt, df, df1)
  colnames(dt2) <- c('Study', 'Design', 'Sheet', 'Correlation', 'N')
  return(dt2)
}


