day_comp_1 <- function(gt_, day_, masts_, log_ = F){
  # Computes the average distance between ground truth and prediction
  # compares a ground truth segments data frame (containing x_mean, y_mean,
  #   f_start, f_end) and a predicted 
  # day_, (vector) which takes the form of a prediction at regular intervals 
  #   (either as strings, in which cases the masts table is needed or in 
  #   coordinates xy named mast_x and mast_y)
  # masts_ is the name (string!) of a data frame with rownames corresponding 
  #   to the entries in day_ and columns "x_mean" and "y_mean"
  # log_ indicates whether the distances should be log10-ed
  
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
                 if(length(weights) != length(dists)) {cat(v_); flush.console()}
                 c(penalty = sum(weights * dists), duration = sum(weights))
               })
  return(sum(res[1, ]) / sum(res[2, ]))
}


eval_pred <- function(ground_truth_, prediction_, masts_, ...){
  # Calls the "evaluate one day" function.
  # ground_truth_ is a named list of dataframes 
  # prediction_ is a dataframe with rownames matching the names of ground_truth
  # masts is the name of a dataframe with the position of the masts
  days_to_test <- intersect(names(ground_truth_), rownames(prediction_))
  dist_vec <- sapply(days_to_test, function(d_){
    day_comp_1(gt_ = ground_truth_[[d_]], day_ = prediction_[d_, ], 
               masts_ = masts_, ...)
  })
  return(dist_vec)
}