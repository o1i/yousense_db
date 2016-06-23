day_comp_1 <- function(doy, gt_, day_, masts_, log_ = F, days_ = NULL){
  # Computes the average distance between ground truth and prediction
  # compares a ground truth segments data frame (containing x_mean, y_mean,
  #   f_start, f_end) and a predicted 
  # day_, (vector) which takes the form of a prediction at regular intervals 
  #   (either as strings, in which cases the masts table is needed or in 
  #   coordinates xy named mast_x and mast_y)
  # masts_ is the name (string!) of a data frame with rownames corresponding 
  #   to the entries in day_ and columns "x_mean" and "y_mean"
  # log_ indicates whether the distances should be log10-ed
  
  # If days_ is provided, only predictions that differ from days_ are used
  if(!is.null(days_)){
    temp <- days_[doy, ] == day_
    ind_remove <- !is.na(temp) & temp
    rm(temp)
  }
  
  # first get the coordinate format irrespective of the input
  if(mode(day_) == "numeric"){
    if(ncol(day_) > nrow(day_)) day_ <- t(day_)
    breaks <- seq(0, 1, l = nrow(day_) + 1)
  }else{
    breaks <- seq(0, 1, l = length(day_) + 1)
    day_ <- get(masts_)[day_, c("mast_x", "mast_y")]
  }
  
  # Then calculate the average distance
  res <- apply(as.matrix(gt_[, c("x_mean", "y_mean", 
                                 "f_start", "f_end")]), 1,
               FUN = function(v_){
                 v_ <- as.numeric(v_)
                 inds <- (which.min(breaks <= v_[3])-1):
                   (which.max(breaks >= v_[4]) - 1)
                 comp <- day_[inds, , drop = F]
                 distfun <- if(log_){
                   function(w_) log10(sqrt(sum(w_^2)))
                 }else{
                   function(w_) sqrt(sum(w_^2))
                 }
                 dists <- apply(as.matrix(comp - rep(v_[1:2], 
                                                     each = nrow(comp)), 
                                          ncol = 2), 
                                1, distfun)
                 weights <- pmin(v_[4], breaks[pmin(inds + 1, length(breaks))])-
                   pmax(v_[3], breaks[inds])
                 if(!is.null(days_)){
                   weights[!is.na(match(inds, which(ind_remove)))] <- 0
                 }
                 if(length(weights) != length(dists)) {cat(v_); flush.console()}
                 c(penalty = sum(weights * dists), duration = sum(weights))
               })
  return(sum(res[1, ]) / sum(res[2, ]))
}

day_comp_2 <- function(doy, gt_, day_, days_){
  # Inputs:
  # gt_    data frame containing the mast labels of the day with the time
  #        intervals they were connected for every slot of the day
  # day_   vector with mast labels
  
  # Outputs: A vector with the following elements
  # Fraction of predicted labels with a positive duration for that slot in gt_
  #   pessimistic, because gt may not fill a slot
  #   + the optimistic version, where incomplete slots are matches
  #   + time weighted version
  # Fraction of predicted labels which occured that day (recall of labels)
  #   + time weighted version
  # Fraction of gt_labels that were in the result
  # For matches: proportion time connected to that mast (0 otherwise)
  # Proportion of connected time of the predicted label for the whole day
  
  outnames <- c("acc_p", "acc_o", "acc_p_t", "acc_o_t", "recall_l", 
                "recall_l_t")
  compfn <- function(gt_, day_, prefix_){
    rownames(gt_) <- gt_$id_masts
    time_connected <- colSums(gt_[, -1]) * length(day_)
    temp <- gt_[cbind(match(day_, gt_$id_masts),1:length(day_))]
    
    # accuracy measures
    accuracy_pess <- sum(temp > 0) / sum(time_connected > 0)
    accuracy_opt <- sum(temp > 0 | temp == 0 & time_connected < 1) / 
      sum(time_connected > 0)
    accuracy_pess_time <- sum(temp) / sum(time_connected)
    accuracy_opt_time <- sum(temp + (1-time_connected)) /  
      sum(time_connected > 0)
    
    # recall measures
    times <- rowSums(gt_[, -1])
    recall_labels <- mean(!is.na(match(gt_$id_masts, day_)))
    recall_labels_time_weighted <- sum(times[intersect(gt_$id_masts, day_)]) / 
      sum(times)
    
    # return
    r <- c(accuracy_pess, accuracy_opt, 
           accuracy_pess_time, accuracy_opt_time,
           recall_labels, recall_labels_time_weighted)
    names(r) <- paste0(prefix_, outnames)
    return(r)
  }
  
  part1 <- compfn(gt_, day_, "")
  gt2 <- gt_
  not_predicted <- days[doy, ] == day_
  gt2[, c(F, !is.na(not_predicted) & not_predicted)] <- 0
  gt2 <- gt2[rowSums(gt2[, -1]) > 0, ]
  part2 <- compfn(gt2, day_, "pred_only_")
  c(part1, part2)
}


eval_pred <- function(ground_truth_, prediction_, func, ...){
  # Calls the "evaluate one day" function.
  # ground_truth_ is a named list of dataframes 
  # prediction_ is a dataframe with rownames matching the names of ground_truth
  # masts is the name of a dataframe with the position of the masts
  days_to_test <- intersect(names(ground_truth_), rownames(prediction_))
  result <- sapply(days_to_test, function(d_){
    func(doy = d_, gt_ = ground_truth_[[d_]], day_ = prediction_[d_, ], ...)
  })
  return(result)
}


