extract_extreme <- function(fn, dt) {
  # fn = the filename of the excel file extracted from covidence
  # dt = the data table listing (1) study name (2) design and (3) corresponding excel 
  # worksheet in the covidence spreadsheet
  
  ## THIS FUNCTION DOES NOT WORK ##
  
  dt2 <- dt[which(dt$Design == 'extreme group'),]
  dt2 <- dt2[which(dt2$Sheet!=20),]
  for (j in 1:nrow(dt2)) {
    print(j)
    sheet = dt2$Sheet[j]
    dt <- read_excel(here('data', fn), sheet = sheet)
    start <- grep('Outcomes',dt$Agreed)[1]
    df <- dt[(start):(start+6),]
    size = ncol(df)
    YA <- df[which(df$Agreed == 'Younger'),]
    OA <- df[which(df$Agreed == 'Older'), ]
   
    if (size == 5 && is.na(df[7,5])) {
      print('normal')
      dt2[j,4] <- YA[2]
      dt2[j,5] <- YA[3]
      dt2[j,6] <- YA[4]
      dt2[j,7] <- OA[2]
      dt2[j,8] <- OA[3]
      dt2[j,9] <- OA[4]
    } else {
      print('find average')
      n_samples = (ncol(df)-2)/2
      dt2[j,4] <- mean(as.numeric(YA[2:(2+(n_samples-1))]), na.rm=TRUE)
      dt2[j,5] <- mean(as.numeric(YA[(2+n_samples):(n_samples*2)+1]), na.rm=TRUE)
    }
  }
  colnames(dt2) <- c('Study', 'Design', 'Sheet', 'YA_M', 'YA_SD', 'YA_N', 'OA_M', 'OA_SD', 'OA_N')
  return(dt2)
}


