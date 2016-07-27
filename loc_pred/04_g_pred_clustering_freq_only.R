pred_cluster_v3 <- function(days_, nt_ = 24, min_pts = 4, 
                         seen_masts_, scale_ = 1, pen_ = 0.5, thresh_ = 0.27,
                         save_stuff = F, eps_ = 0.05, minpts_ = 5){
 
 # --- Distance functions needed for clustering
 mast_dist <- function(m1, m2, seen_masts_, a = 2.6289316954, b = 0.0002054995,
                       min_avg_dist = 300){
   if(any(is.na(c(m1, m2)))) return(NA)
   avgs <- c(NA, NA)
   d1 <- seen_masts_[m1, c("mast_x", "mast_y")]
   avgs[1] <- max(min_avg_dist, seen_masts_[m1, "neighbour_avg_dist"])
   d2 <- seen_masts_[m2, c("mast_x", "mast_y")]
   avgs[2] <- max(min_avg_dist, seen_masts_[m2, "neighbour_avg_dist"])
   d <- sqrt(sum((d1 - d2)^2))
   avgs[is.na(avgs)] <- avgs[!is.na(avgs)]
   t1 <- a * avgs[1] - b * avgs[1]^2
   t2 <- a * avgs[2] - b * avgs[2]^2
   if(d < t1 & d < t2) return(-1)
   if(d < t1 | d < t2) return(-0.5)
   #if(d < 3 * t1 | d < 3 * t2) return(0)
   # if(d > 20000) return(2)
   # return(1)
   return(log(d) / log(100))
 }
 # mast_dist <- function(m1, m2, ...){
 #   if(any(is.na(c(m1, m2)))) return(NA)
 #   return(-2 * (m1 == m2) + 1)
 # }
 day_dist_simple <- function(day1, day2){
   apply(cbind(day1, day2), 1, function(v) mast_dist(v[1], v[2], 
                                                 seen_masts_ = seen_masts_))
 }
 day_dist <- function(day1, day2, pen_ = 0.5, scale_ = 1){
   dists1 <- cbind(day_dist_simple(day1, c(day2[1], day2[-length(day2)])) + pen_,
                  day_dist_simple(day1, day2),
                  day_dist_simple(day1, c(day2[-1], day2[length(day2)])) + pen_)
   dists2 <- cbind(day_dist_simple(c(day1[1], day1[-length(day1)]), day2) + pen_,
                   day_dist_simple(day1, day2),
                   day_dist_simple(c(day1[-1], day1[length(day1)]), day2) + pen_)
   return((sigmoid(scale_ * sum(apply(dists1, 1, function(v) {
     if(any(!is.na(v))){
       min(v, na.rm = T)
     }else{
       0
     }
   }))) + 
     sigmoid(scale_ * sum(apply(dists2, 1, function(v) {
       if(any(!is.na(v))){
         min(v, na.rm = T)
       }else{
         0
       }
     })))) / 2
   )
 }
 num_of_mismatches <- function(day1, day2){
   a <- day1 != day2
   return(sum(a, na.rm = T))
 }
 
 # --- Calculating distances
 frequent_days <- subset(days_, 
                         apply(days, 1, function(v) sum(!is.na(v)) >=min_pts))
 if(nrow(frequent_days) > 0){
   distmat <- matrix(NA, nrow = nrow(frequent_days), ncol = nrow(frequent_days))
   n <- nrow(frequent_days)
   ind <- 1:n^2
   ind <- ind[((ind - 1) %% n) + 1 > ((ind - 1) %/% n + 1)]
   distmat[ind] <- unlist(mclapply(ind, FUN = function(i_) {
     day_dist(day1 = frequent_days[((i_ - 1) %% n) + 1, ], 
              day2 = frequent_days[((i_ - 1) %/% n + 1), ], 
              pen = pen_, 
              scale_ = 1)
   }, mc.cores = 4))
   
   if(save_stuff) jpeg(width = 500, height = 500, quality = 100, 
                       file = paste0("figures/byuser/distmat_u", user, "nt_", nt_, "v3.jpeg"))
   image(t(distmat), main = paste("User", user, "nt =", nt_), zlim = c(0, 1), ylim = c(1, 0),
         xaxt = 'n', yaxt = 'n')
   if(save_stuff) dev.off()
   
   
   # --- Clustering
   library(dbscan)
   dm <- distmat
   cl <- numeric(nrow(frequent_days))
   ind <- 1:nrow(frequent_days)
   for(eps_ in c(0.005, 0.01, 0.02, 0.03, 0.04, 0.05)){
     clusters <- dbscan(as.dist(dm), eps = eps_, minPts = minpts_)
     ind_clustered <- F
     if(any(clusters$cluster > 0)){
       ind_clustered <- clusters$cluster > 0
       cl[ind[ind_clustered]] <- clusters$cluster[ind_clustered] + max(cl)
       ind <- ind[!ind_clustered]
       dm <- dm[!ind_clustered, !ind_clustered]
     }
     if(all(ind_clustered)) break
   }
   
   # --- Typical days
   typical_days <- apply(sapply(sort(unique(cl)), function(cl_){
     apply(as.matrix(frequent_days)[cl == cl_, ,drop = F], 2, Mode, minPts_ = 2)
   }), 2, fill_vector)
   typical_days

 
 
 # --- Match days to clusters
 dists_to_cluster <- num_mismatch <- matrix(NA, nrow = nrow(days_), 
                                            ncol = ncol(typical_days))
 ind <- 1:(nrow(days_) * ncol(typical_days))
 dists_to_cluster[ind] <- unlist(mclapply(ind, FUN = function(i_) {
   day_dist(days_[((i_ - 1) %% nrow(days)) + 1, ], 
            typical_days[, ((i_ - 1) %/% nrow(days) + 1)])
 }, mc.cores = 4))
 num_mismatch[ind] <- unlist(mclapply(ind, FUN = function(i_) {
   num_of_mismatches(days_[((i_ - 1) %% nrow(days)) + 1, ], 
            typical_days[, ((i_ - 1) %/% nrow(days) + 1)])
 }, mc.cores = 4))
 cluster <- apply(dists_to_cluster + (num_mismatch > nt_/6), 1, FUN = function(v){
   if(any(v < thresh_)){which.min(v) - 1}else{0}
 })
 }else{
   typical_days <- matrix(NA, ncol = 1, nrow = ncol(days_))
   cluster <- rep(0, nrow(days_))
 }
 
 # --- Predict
 predictions <- t(typical_days[, cluster + 1])
 predictions[!is.na(days_)] <- days_[!is.na(days_)]
 # zeros are just extended
 predictions[cluster == 0, ] <-  t(apply(days_[cluster == 0, ], 1, fill_vector))
 rownames(predictions) <- rownames(days_)
 return(predictions)
}

