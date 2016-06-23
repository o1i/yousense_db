pred_freq <- function(days_){
  library(stringr)
  library(arules)
  sink("/dev/null")
  days_df <- as.data.frame(days_)
  days_df_dow <- doy_to_dow(y = 2015, rownames(days_df))
  
  days_t <- as(days_df, "transactions")
  suppressWarnings({
    rules = apriori(days_t, parameter=list(support=2/nrow(days_df), 
                                           confidence=0.5));
  })

  for(i in 1:nrow(days_df)){
    to_test <- as(days_df[i, ], "transactions")
    suitableRules <- is.subset(rules@lhs,to_test) & 
      !(is.subset(rules@rhs,to_test))
    if(any(suitableRules)){
      to_apply <- inspect(rules[suitableRules])
      rules_alone <- str_extract_all(to_apply$rhs, "[0-9]+") %>%
        do.call(what = rbind)  %>% 
        as.numeric() %>% matrix(ncol = 2) %>%
        cbind(to_apply$lift) %>% as.data.frame()
      colnames(rules_alone) <- c("time", "id_mast", "lift")
      changes <- sapply(unique(rules_alone$time), function(t_){
        df_ <- subset(rules_alone, time == t_)
        c(t_, df_[which.max(df_$lift), "id_mast"])
      })
      ind <- is.na(days_df[i, as.character(changes[1, ])])
      days_df[i, as.character(changes[1, ])[ind]] <- changes[2, ind]
    }
  }
  sink()
  return(t(apply(days_df, 1, FUN = fill_vector)))
}

