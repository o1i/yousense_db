pred_cluster <- function(days_, nt_ = 24, num_clus = 5){
  library(plyr)
  library(parallel)
  
  # Distance function between masts
  mast_dist <- function(m1, m2, probs = T, mult_ = 300, avg_method_ = mean){
    if(any(is.na(c(m1, m2)))) return( 0.3)
    # Measures "probabilities" of both masts connected at the same gps-location
    #   or distances if at least one mast is given.
    avgs <- c(NA, NA)
    if(mode(m1) == "character"){
      d1 <- seen_masts[m1, c("mast_x", "mast_y")]
      avgs[1] <- seen_masts[m1, "neighbour_avg_dist"]
    }else{
      d1 <- m1
    }
    if(mode(m2) == "character"){
      d2 <- seen_masts[m1, c("mast_x", "mast_y")]
      avgs[2] <- seen_masts[m2, "neighbour_avg_dist"]
    }else{
      d2 <- m2
    }
    d <- sqrt(sum((d1 - d2)^2))
    if(probs & !all(is.na(avgs))){
      avgs[is.na(avgs)] <- avgs[!is.na(avgs)]
      avg_method_(c(sigmoid((d- (2.6289316954 * avgs[1] - 
                                   0.0002054995 * avgs[1]^2))/mult_),
                    sigmoid((d- (2.6289316954 * avgs[2] - 
                                   0.0002054995 * avgs[2]^2))/mult_)))
    }else{
      d
    }
  }
  
  get_standard_days <- function(days_, clusters_, coords = T){
    # takes a bunch of clustered days and returns a list of representative days
    if(length(clusters_) != nrow(days_)) {
      stop("length of cluster assignments must be equal to number of days")
    }
    dl <- lapply(as.list(unique(clusters_)), 
                 function(cl_) days_[clusters_ == cl_, , drop = F])
    names(dl) <- unique(clusters_)
    lapply(dl, function(arr){
      if(coords){
        stop("not yet implemented")
        # return(t(colMeans(seen_masts[v[!is.na(v)], c("mast_x", "mast_y")])))
      }else{
        fill_vector(apply(arr, 2, Mode))
      }
    })
  }
  
  get_relevant_value <- function(v) {
    if(min(v, na.rm = T)<0.4){min(v, na.rm = T)}else{v[which.max(abs(v-0.5))]}
  }
  
  dist_fun_4 <- function(d1, d2, value_mat_, mult_, method_){
    mask <- outer(d1, d2, FUN = function(v1, v2) !is.na(v1) & !is.na(v2)) * 
      value_mat_
    if(sum(mask) == 0){
      return(0.5)
    }else{
      filled <- which(mask>0, arr.ind = T)
      scores <- apply(filled, 1, FUN = function(v){
        mast_dist(m1 = d1[v[1]], m2 = d2[v[2]], probs = T, mult_, method_)
      })
      score_1 <- mask[filled] * scores + (1-mask[filled]) * 0.5
      score_2 <- c(aggregate(score_1, by = list(filled[, 1]), 
                             FUN = get_relevant_value)$x,
                   aggregate(score_1, by = list(filled[, 2]), 
                             FUN = get_relevant_value)$x,
                   rep(0.5, 2 * nt_ - length(unique(filled[, 1])) - 
                         length(unique(filled[, 2]))))
      w <- 1 - (0.95 * (score_2 == 0.5))
      return(sum(score_2 * w) / sum(w))
    }
  }
  
  value_mat_4 <- abs(1 * outer(1:nt_, 1:nt_, "-"))
  value_mat_4 <- (value_mat_4 < 2) + (0.5* (value_mat_4 == 2))
  
  
  
  distmat <- matrix(NA, nrow = nrow(days_), ncol = nrow(days_))
  n <- nrow(days_)
  ind <- 1:n^2
  ind <- ind[((ind - 1) %% n) + 1 > ((ind - 1) %/% n + 1)]
  # unlist(sapply(ind[1:300], FUN = function(i_) {
  #   cat(paste(i_, "\n")); flush.console();
  #   dist_fun_4(d1 = days_[((i_ - 1) %% n) + 1, ],
  #              d2 = days_[((i_ - 1) %/% n + 1), ],
  #              value_mat_ = value_mat_4,
  #              mult_ = 300,
  #              method_ = mean)
  # }))
  distmat[ind] <- unlist(mclapply(ind, FUN = function(i_) {
    dist_fun_4(d1 = days_[((i_ - 1) %% n) + 1, ], 
               d2 = days_[((i_ - 1) %/% n + 1), ], 
               value_mat_ = value_mat_4, 
               mult_ = 300, 
               method_ = mean)
  }, mc.cores = 4))
  
  
  diag(distmat) <- apply(days_, 1, FUN = function(v) sum(!is.na(v))/nt_)
  jpeg(width = 500, height = 500, quality = 100, 
       file = paste0("figures/byuser/distmat_u", user, ".jpeg"))
  image(distmat, main = paste("User", user))
  dev.off()
  dists <- as.dist(distmat)
  temp <- hclust(dists, "ward.D2")
  clus_labels <- cutree(temp, k = num_clus)
  
  prototype_days <- get_standard_days(days_, clus_labels, coords = F)
  
  better1 <- t(apply(days_, 1, function(d_){
    # assumes both d and the prototype days to be masts
    propose <- prototype_days[[which.min(sapply(prototype_days, function(ptd){
      daywarp(d1 = d_, d2 = ptd, d = mast_dist, w = 2, probs = T)
    })
    )]]
    propose[!is.na(d_)] <- d_[!is.na(d_)]
    return(propose)
  }))
  rownames(better1) <- rownames(days_)
  return(better1)
}