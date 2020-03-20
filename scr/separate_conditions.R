separate_conditions <- function(data, id, conds, namebase) {
  # data = data from covidence
  # id = study name (Study.Identifier column)
  # conds = vector of numbers that distinguish conditions from each other in covidence data frame
  # namebase = the characters that are common in the names of all conditions
  dt <- data[which(data$Study.Identifier == id), ] # isolate study data
  dt <- dt[,colSums(is.na(dt)) == 0] # remove NA columns
  cols <- paste0(namebase, conds)
  dt <- gather(dt, condition, measure, cols)
  dt$condition <- gsub(namebase, '', dt$condition)
  return(dt)
}
