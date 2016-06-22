pred_cluster_v2 <- function(days_, seen_masts_, eps_m = 0.3, minpt_m = 10, 
                            eps_c = 4.5, minpt_c = 10){
  # This version first clusteres the masts using dbscan
  seen_masts_$epsdist <- pmax(0.5, 2.6289316954 - 
    0.0002054995 * seen_masts_$neighbour_avg_dist) * 
    seen_masts_$neighbour_avg_dist
  all_conn <- days_[!is.na(days_)]
  mast_dists <- matrix(NA, ncol = length(all_conn), nrow = length(all_conn))
  n <- length(all_conn)
  ind <- 1:n^2
  ind <- ind[((ind - 1) %% n) + 1 > ((ind - 1) %/% n + 1)]
  mast_dists[ind] <- unlist(mclapply(ind, FUN = function(k_) {
    i_ <- ((k_ - 1) %% n) + 1
    j_ <- ((k_ - 1) %/% n + 1)
    sqrt(sum((seen_masts_[all_conn[i_], 
                         c("mast_x", "mast_y")] -
               seen_masts_[all_conn[j_], c("mast_x", "mast_y")])^2)) / (
                 seen_masts_[all_conn[i_], "epsdist"] + 
                   seen_masts_[all_conn[j_], "epsdist"]
               )
  }, mc.cores = 4))
  
  mast_clust <- dbscan(as.dist(mast_dists), eps = 0.15, minPts = minpt_m)
  mast_clust_no <- mast_clust$cluster[match(rownames(seen_masts_), all_conn)] + 2
  mast_clust_no[is.na(mast_clust_no)] <- 1
  names(mast_clust_no) <- rownames(seen_masts_)
  
  # rm(all_conn, mast_dists, n, ind)
  # cols <- colorRampPalette(c("black", "yellow", "green", "blue", "orange", "red"))(24)
  # locs <- SpatialPoints(seen_masts_[, c("mast_x", "mast_y")], CRS("+init=epsg:3301")) %>%
  #   spTransform( CRS("+init=epsg:4326"))
  # cols <- c("red", "black", brewer.pal(length(unique(mast_clust_no)) - 2, "Set3"))
  # leaflet() %>% addTiles() %>% addCircleMarkers(data = locs, color = cols[mast_clust_no], opacity = 1,
  #                                               popup = rownames(seen_masts_))
  
  # Add "masts" as cluster centres
  add <- t(sapply(unique(mast_clust_no[mast_clust_no > 2]), function(no_){
    c(no_, mean(seen_masts_[match(no_, mast_clust_no), "mast_x"]),
      mean(seen_masts_[match(no_, mast_clust_no), "mast_y"]), 0, 0)
  }))
  rownames(add) <- unique(mast_clust_no[mast_clust_no > 2])
  colnames(add) <- colnames(seen_masts_)
  seen_masts_ <- rbind(seen_masts_, add)
  
  # Replace masts by cluster_id
  all_conn[!is.na(mast_clust_no[all_conn]) & mast_clust_no[all_conn] > 2] <- 
    mast_clust_no[all_conn][mast_clust_no[all_conn] > 2]
  days_[!is.na(days_)] <- all_conn
  
  
  
  library(plyr)
  library(parallel)
  library(dbscan)
  
  # Distance function between masts
  mast_dist <- function(m1, m2){
    m1 <- as.numeric(m1) * (as.numeric(m1) < 100)
    m2 <- as.numeric(m2) * (as.numeric(m2) < 100)
    if(any(is.na(c(m1, m2)))) return(1/3)
    if(m1 == m2){return(0)}else{return(1)}
  }
  
  get_standard_days <- function(days_, clusters_){
    # takes a bunch of clustered days and returns a list of representative days
    # does not return anything for cluster zero
    if(length(clusters_) != nrow(days_)) {
      stop("length of cluster assignments must be equal to number of days")
    }
    dl <- lapply(as.list(unique(clusters_)), 
                 function(cl_) days_[clusters_ == cl_, , drop = F])
    names(dl) <- unique(clusters_)
    mclapply(dl[names(dl) != "0"], function(arr){
      arr[!is.na(arr) & as.numeric(arr) > 1000] <- 0
      try <- fill_vector(apply(arr, 2, Mode))
      possibilities <- sort(unique(arr[1:length(arr)]), na.last = F)
      tot_dist <- Inf
      tot_dist_last <- Inf
      counter <- 0
      while((tot_dist != tot_dist_last & counter > 0 & counter < 5) | 
            counter == 0){
        tot_dist_last <- tot_dist
        for(pos in 1:length(try)){
          for(p_ in possibilities){
            test <- try
            test[pos] <- p_
            test_dist <- sum(apply(arr, 1, FUN = daywarp, d2 = test, d = mast_dist, w = 1,
                     k =  function(i, j) {(i!=j) * 1/3}))
            if(test_dist < tot_dist){
              tot_dist <- test_dist
              try[pos] <- p_
            }
          }
        }
        counter <- counter + 1
      }
      print(counter)
      out <- fill_vector(try)
      names(out) <- names(test)
      return(out)
    }, mc.cores = 4)
    
  }
  
  ind_used <- apply(days_, 1, function(v) sum(!is.na(v))>1)
  days_used <- days_[ind_used, ]
  distmat <- matrix(NA, nrow = nrow(days_used), ncol = nrow(days_used))
  n <- nrow(days_used)
  ind <- 1:n^2
  ind <- ind[((ind - 1) %% n) + 1 > ((ind - 1) %/% n + 1)]
  # unlist(sapply(ind[1:300], FUN = function(i_) {
  #   cat(paste(i_, "\n")); flush.console();
  #   dist_fun_4(d1 = days_used[((i_ - 1) %% n) + 1, ],
  #              d2 = days_used[((i_ - 1) %/% n + 1), ],
  #              value_mat_ = value_mat_4,
  #              mult_ = 300,
  #              method_ = mean)
  # }))
  distmat[ind] <- unlist(mclapply(ind, FUN = function(i_) {
    daywarp(d1 = days_used[((i_ - 1) %% n) + 1, ], 
               d2 = days_used[((i_ - 1) %/% n + 1), ], 
               d = mast_dist, 
               w = 1, 
               k = function(i, j) {(i!=j) * 1/3})
  }, mc.cores = 4))
  
  image(distmat, main = paste("User", user))
  dists <- as.dist(distmat)
  eps_c <- 7.5; minpt_c <- 30
  temp <- dbscan(dists, eps = eps_c, minPts = minpt_c)
  table(temp$cluster)
  clus_labels <- temp$cluster
  dow <- weekdays(strptime(paste(2015, rownames(days_used)), format = "%Y %j"))
  freqs <- table(clus_labels, dow)
  freqs <- freqs[rownames(freqs) != "0", ]
  priors <- (freqs + 1) / rep(apply(freqs + 1, 2, sum), each = nrow(freqs))
  
  # sats <- which(dow == "Sunday")
  # m <- NULL
  # for(i_ in sats){
  #   m <- plot_day(days_used[i_, ], seen_masts_, m, label = rownames(days_used)[i_])
  # }
  
  
  # uu1 <- c(131, 138, 144, 136, 140, 137, 139)
  # uu2 <- distmat[uu1, uu1]
  # uu2[is.na(uu2)] <- 0
  # uu3 <- uu2 + t(uu2)
  # uu3[uu3 == 0] <- NA
  # image(uu3)
  # days_used[uu1, ]
  
  prototype_days <- get_standard_days(days_used, clus_labels)
  
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