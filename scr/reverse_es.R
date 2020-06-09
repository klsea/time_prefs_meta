reverse_es <- function(df, studyid) {
  x = df[which(df$Study.Identifier == studyid),] # pull out study of interest
  x$effect_size <- x$effect_size * -1 # reverse effect size
  dt <- df[-which(df$Study.Identifier == studyid),] # remove old effect sizes from df
  rbind(dt, x) # add new effectsizes to df
}