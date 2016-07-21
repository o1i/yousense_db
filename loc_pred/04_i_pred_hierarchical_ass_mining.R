pred_freq_hierarchical <- function(days_){
  library(stringr)
  library(arules)
  sink("/dev/null")
  #days_df <- as.data.frame(days_)
  days_df_dow <- substr(doy_to_dow(y = 2015, rownames(days_)), 1, 3)
  days_wd <- matrix(paste0(days_df_dow, "_", days_), ncol = ncol(days_),
                          dimnames = list(rownames(days_), 
                                          paste0("wd_", colnames(days_))))
  days_wd[is.na(days_)] <- NA
  days_df <- cbind(as.data.frame(days_), as.data.frame(days_wd))
  
  days_t <- as(days_df, "transactions")
  suppressWarnings({
    rules = apriori(days_t, parameter=list(support=3/length(days_t), 
                                           confidence=0.5));
    rule_labels <- labels(rules)
    useful <- str_extract_all(rule_labels, "[0-9]+") %>% sapply(function(v){
      v2 <- rev(v[(1:length(v)) %% 2 != 0])
      !(v2[1] %in% v2[-1])
    })
    rules <- subset(rules, useful)
  })

  for(i in 1:nrow(days_df)){
    day_ <- days_df_dow[i]
    to_test <- as(days_df[i, ], "transactions")
    suitableRules <- is.subset(rules@lhs,to_test) & 
      !(is.subset(rules@rhs,to_test))
    if(any(suitableRules)){
      to_apply <- inspect(rules[suitableRules])
      ind <- grepl(day_, to_apply$rhs, fixed = T) |
        !grepl("[a-zA-Z]{3}", to_apply$rhs)
      if(!any(ind)) break
      to_apply <- subset(to_apply, ind)
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
  return(t(apply(days_df[, 1:(ncol(days_df) / 2)], 1, FUN = fill_vector)))
}

