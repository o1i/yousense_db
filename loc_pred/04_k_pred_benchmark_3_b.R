bench3b <- function(days_){
  # Mode every day of the week
  dow <- weekdays(strptime(paste(2015, rownames(days_)), format = "%Y %j"))
  dow[dow %in% c("Saturday", "Sunday")] <- "Weekend"
  dow[!dow == "Weekend"] <- "Weekday"
  res <- t(sapply(unique(dow), function(d_){
    fill_vector(apply(days[dow == d_, , drop = F], 2, Mode))
  }))[dow, ]
  rownames(res) <- rownames(days_)
  res[!is.na(as.matrix(days_))] <- days[!is.na(as.matrix(days_))]
  return(res)
}