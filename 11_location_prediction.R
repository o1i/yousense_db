# This script applies the other functions in the folder in order to get
#   predictions.
rm(list=ls())

# --- Set Parameters -----------------------------------------------------------
nt <- 12  # Number of parts to split a day into.
hour_shift <- 3
library(reshape2)
library(plyr)
source("00_defs_funcs.R")
lapply(list.files("loc_pred", full.names = T), source)
library(leaflet)
library(parallel)
library(dbscan)

connect(user = "burkhard", 
        pw = scan("nosafe.txt", what = "character"), 
        dbname = "burkhard",
        host = "localhost", 
        port = 5432)

results <- list()
user_chars <- get_empty_user_char()
daily_losses <- list()
users <- sort(
  dbGetQuery(con, "select distinct uid from user_characteristics;")[, 1])
for(user in users){
  print(user)

  tryCatch({
    # --- Get user days --------------------------------------------------------
    temp <- get_user_days_c(user, nt_ = nt, level_ = "CDR", hour_shift = hour_shift)
    seen_masts <- temp[["seen_masts"]]
    days <- temp[["days"]]
    clus_polys <- temp[["polys"]]
    rm(temp)
    
    # nrow(days)
    # leaflet()%>%addTiles()%>%addPolygons(data = spTransform(clus_polys, CRS("+init=epsg:4326")))
    
    # --- Create Predictions ---------------------------------------------------
    p_bench1 <- bench1(days)
    p_bench2 <- bench2(days)
    p_bench3 <- bench3(days)
    p_cluster <- pred_cluster(days_ = days, nt_ = nt, num_clus = 5)
    # p_cluster_v1 <- pred_cluster_v1(days_ = days)
    p_freq <- pred_freq(days)
    
    # --- Get Ground Truth data ------------------------------------------------
    gt <- get_segments_gt(user, hour_shift = hour_shift, include_moves = F)
    gt_masts <- get_segments_gt_masts(user, seen_masts_ = seen_masts, 
                                      nt_ = nt, hour_shift = hour_shift)
    
    # --- Evaluate the predictions ---------------------------------------------
    eval_dist_all <- lapply(list(p_bench1, p_bench2, p_bench3, p_cluster, 
                                 p_freq), function(pred){
                                   eval_pred(ground_truth_ = gt,
                                             prediction_ = pred,
                                             func = day_comp_1,
                                             masts_ = "seen_masts", log_ = T)
                                 })
    names(eval_dist_all) <- c("s_bench1", "s_bench2", "s_bench3", "s_cluster",
                              "s_freq")
    
    eval_dist_pred_only <- lapply(list(p_bench1, p_bench2, p_bench3, p_cluster, 
                                       p_freq), function(pred){
                                         eval_pred(ground_truth_ = gt,
                                                   prediction_ = pred,
                                                   func = day_comp_1,
                                                   masts_ = "seen_masts", 
                                                   log_ = T,
                                                   days_ = days)
                                       })
    names(eval_dist_pred_only) <- c("s_bench1", "s_bench2", "s_bench3", "s_cluster",
                              "s_freq")
    
    eval_labels <- lapply(list(p_bench1, p_bench2, p_bench3, p_cluster, 
                               p_freq), function(pred){
                                 t(eval_pred(ground_truth_ = gt_masts,
                                           prediction_ = pred,
                                           func = day_comp_2,
                                           days_ = days))
                               })
    names(eval_labels) <- c("s_bench1", "s_bench2", "s_bench3", "s_cluster",
                              "s_freq")
    
    # print("Evaluation complete")
    
    # --- store Results --------------------------------------------------------
    # --- numbers
    user_chars <- rbind(user_chars, user_char(days, gt, user))
    save(user_chars, file = "results/user_chars.rda")
    daily_losses[[as.character(user)]] <- list(dist_all = eval_dist_all,
                                               dist_pred = eval_dist_pred_only,
                                               labels = eval_labels)
    save(daily_losses, file = "results/daily_losses.rda")
    
    # --- images
    x <- seq(2, 5, l = 100)
    jpeg(height = 500, width = 500, quality = 100, 
         file = paste0("figures/byuser/pred_comp_all_u", user, ".jpeg"))
    plot(x, ecdf(eval_dist_all[["s_bench1"]])(x), type = "l", main = paste0("Comp, user ", user),
         ylab = "ecdf of days below a certain avg error",
         xlab = "log10 of distance")
    lines(x, ecdf(eval_dist_all[["s_bench2"]])(x), col = 2)
    lines(x, ecdf(eval_dist_all[["s_bench3"]])(x), col = 3) 
    lines(x, ecdf(eval_dist_all[["s_cluster"]])(x), col = 4)
    lines(x, ecdf(eval_dist_all[["s_freq"]])(x), col = 5, lty = 2)
    dev.off()
    
    jpeg(height = 500, width = 500, quality = 100, 
         file = paste0("figures/byuser/pred_comp_predonly_u", user, ".jpeg"))
    plot(x, ecdf(eval_dist_pred_only[["s_bench1"]])(x), type = "l", main = paste0("Comp, user ", user),
         ylab = "ecdf of days below a certain avg error",
         xlab = "log10 of distance")
    lines(x, ecdf(eval_dist_pred_only[["s_bench2"]])(x), col = 2)
    lines(x, ecdf(eval_dist_pred_only[["s_bench3"]])(x), col = 3) 
    lines(x, ecdf(eval_dist_pred_only[["s_cluster"]])(x), col = 4)
    lines(x, ecdf(eval_dist_pred_only[["s_freq"]])(x), col = 5, lty = 2)
    dev.off()
    
    vis_gt(user_ = user, eps_coords = 30, minpts_coords = 4, nt_ = nt,
           hour_shift = hour_shift)
    
    
    

  },
  error = function(e){print(paste("Error at user", user, "\n", e))},
  warning = function(w){print(paste("Warning at user", user, "\n", w))}
  )
}

