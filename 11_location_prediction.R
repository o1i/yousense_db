# This script applies the other functions in the folder in order to get
#   predictions.
tryCatch(disconnect(), error = function(e){})
rm(list=ls())

# ------------------------------------------------------------------------------
# --- Set Parameters 
# ------------------------------------------------------------------------------

nt <- 12  # Number of parts to split a day into.
for(nt in c(12, 24, 6)){
hour_shift <- 3
library(reshape2)
library(plyr)
source("00_defs_funcs.R")
lapply(list.files("loc_pred", full.names = T), source)
library(leaflet)
library(parallel)
library(dbscan)

connect(pw = scan("nosafe.txt", what = "character"))

initialise <- function(obj_name, filename, init){
  if(file.exists(filename)){
    load(filename)
    print(paste(filename, "loaded"))
    return(get(obj_name))
  }else{
    init
  }
}

user_chars <- initialise("user_chars", paste0("results/user_chars_", 
                                              hour_shift, "_", nt, 
                                ".rda"), get_empty_user_char())
all_evals <-  initialise("all_evals", paste0("results/all_evals_", 
                                           hour_shift, "_", nt, 
                                ".rda"), list())
places_info <-  initialise("places_info", paste0("results/places_info_", 
                                                 hour_shift, "_", nt, 
                                  ".rda"), list())
daily_infos <- initialise("daily_infos", paste0("results/daily_infos_", 
                                              hour_shift, "_", nt, 
                                ".rda"), list())

users <- sort(
  dbGetQuery(con, "select distinct uid from user_characteristics;")[, 1])
force_fit <- F
save_stuff <- T
eval_stuff <- F


for(user in users){
  tryCatch(sink(), 
           error = function(e){},
           warning = function(w){}
  )
  cat(paste("User", user, ": ", Sys.time(), " "))
  
  cat(paste(round(system.time(tryCatch({
    # --------------------------------------------------------------------------
    # --- Get user days 
    # --------------------------------------------------------------------------
    filename <- paste0("results/intermediate/train_u", user, "_nt", 
      nt, ".rda")
    if(!file.exists(filename) | force_fit){
      temp <- get_user_days_c(user_ = user, nt_ = nt, level_ = "CDR", 
                              hour_shift = hour_shift)
      seen_masts <- temp[["seen_masts"]]
      days <- temp[["days"]]
      clus_polys <- temp[["polys"]]
      rm(temp)
      if(save_stuff) save(seen_masts, days, clus_polys, file = filename)
    }else{
      load(filename)
    }
    
    # nrow(days)
    # leaflet() %>% addTiles() %>% 
    #   addPolygons(data = spTransform(clus_polys, CRS("+init=epsg:4326")))
    
    # --------------------------------------------------------------------------
    # --- Create Predictions 
    # --------------------------------------------------------------------------
    days <- days[which.max(as.numeric(rownames(days)) < 300):nrow(days), ]
    filename <- paste0("results/fits/fits_", hour_shift, "_", nt,
                       "_", user, ".rda")
    if(force_fit | !file.exists(filename)){
      p_bench1   <- bench1(days)
      p_bench1b  <- bench1b(days)
      p_bench2   <- bench2(days)
      p_bench3   <- bench3(days)
      p_bench3b  <- bench3b(days)
      p_bench4   <- bench4(user_ = user, nt2 = 12 * 24, masts_ = "seen_masts")
      p_cluster  <- pred_cluster(days_ = days, nt_ = nt, num_clus = 5)
      p_cluster2 <- pred_cluster_v3(days_ = days, 
                                    nt_ = nt, 
                                    min_pts = 3, 
                                    seen_masts_ = seen_masts, 
                                    scale_ = 1, 
                                    pen_ = 0.5, 
                                    thresh_ = 0.5,
                                    save_stuff = save_stuff, 
                                    eps_ = 0.05, 
                                    minpts_ = 4)
      # p_cluster_v1 <- pred_cluster_v1(days_ = days)
      p_freq <- pred_freq(days)
      p_freq_h <- pred_freq_hierarchical(days)
      if(save_stuff)  save("p_bench1", "p_bench1b", "p_bench2", "p_bench3",
                           "p_bench3b", "p_bench4",
                           "p_cluster",
                           "p_cluster2", "p_freq", "p_freq_h", file = filename)
    }else{
      load(filename)
    }
    
    # --------------------------------------------------------------------------
    # --- Get Ground Truth data 
    # --------------------------------------------------------------------------
    #gt <- get_segments_gt(user, hour_shift = hour_shift, include_moves = F)
    gt_raw <- get_raw_gt_gps(user_ = user, hour_shift = hour_shift)
    gt_masts <- get_segments_gt_masts(user, seen_masts_ = seen_masts, 
                                      nt_ = nt, hour_shift = hour_shift)
    gt_masts <- subset(gt_masts, names(gt_masts) %in% names(gt_raw))
    
    # --------------------------------------------------------------------------
    # --- Evaluate the predictions
    # --------------------------------------------------------------------------
    if(eval_stuff){
      # --- Evaluation against raw gps
      eval_dist <- lapply(list(p_bench1, p_bench1b, p_bench2, p_bench3, 
                               p_bench3b,
                               p_bench4, p_cluster, 
                               p_cluster2, p_freq, p_freq_h), 
                          function(pred){
                            eval_pred(
                              ground_truth_ = gt_raw,
                              prediction_ = pred,
                              func = day_comp_1,
                              masts_ = "seen_masts", 
                              give_days = T,
                              days_ = days)
                          })
      names(eval_dist) <- c("s_bench1", "s_bench1b", "s_bench2", "s_bench3", 
                            "s_bench3b","s_bench4",
                            "s_cluster", "s_cluster2", "s_freq", "s_freq_h")
      
      # --- Maximally possible accuracy
      eval_handover <- day_comp_handover(gt_ = gt_raw)
      
      
      # --- Evaluation on the label level
      eval_labels <- lapply(list(p_bench1, p_bench1b, p_bench2, p_bench3, 
                                 p_bench3b, p_cluster, 
                                 p_cluster2, p_freq, p_freq_h), function(pred){
                                   eval_pred(ground_truth_ = gt_masts,
                                             prediction_ = pred,
                                             func = day_comp_2,
                                             days_ = days,
                                             give_days = F
                                   )
                                 }) 
      names(eval_labels) <- c("s_bench1", "s_bench1b", "s_bench2", "s_bench3", 
                              "s_bench3b", "s_cluster",
                              "s_cluster2", "s_freq", "s_freq_h")
      
      # ------------------------------------------------------------------------
      # --- Context Information
      # ------------------------------------------------------------------------
      
      # --- Information on the days, to put results into perspective
      temp <- t(apply(days, 1, FUN = function(v){
        v <- unname(v)
        c(first = (which.max(!is.na(v))-1)/nt, 
          count = sum(!is.na(v)),
          last = (nt - which.max(!is.na(rev(v))))/nt)
      }))
      daily_info <- data.frame(dow = doy_to_dow(2015,rownames(days)),
                               temp)
      
      # --- Create the outfile
      all_eval <- c(sapply(names(eval_labels), function(n_){
        cbind(eval_dist[[n_]], eval_labels[[n_]])
      }, simplify = F), 
      list(s_bench4 = eval_dist$s_bench4),
      list(eval_days = eval_dist[[1]]$eval_days), 
      list(handover = eval_handover))
      
      # ------------------------------------------------------------------------
      # --- store Results
      # ------------------------------------------------------------------------
      # file should be called daily_infos_... change next time.
      daily_infos[[as.character(user)]] <- daily_info
      if(save_stuff) save(daily_infos, file = paste0("results/daily_infos_", 
                                                     hour_shift,
                                                     "_", nt, ".rda"))
      # --- numbers
      user_chars[as.character(user), ] <- user_char(days, gt_raw, user)
      if(save_stuff) save(user_chars, file = paste0("results/user_chars_", 
                                                    hour_shift,
                                                    "_", nt, ".rda"))
      all_evals[[as.character(user)]] <- all_eval
      if(save_stuff) save(all_evals, 
                          file = paste0("results/all_evals_", 
                                        hour_shift, "_", nt, ".rda"))
    }else{
      load(paste0("results/daily_infos_", hour_shift, "_", nt, ".rda"))
      load(paste0("results/user_chars_",  hour_shift, "_", nt, ".rda"))
      load(paste0("results/all_evals_",   hour_shift, "_", nt, ".rda"))
      daily_info <- daily_infos[[as.character(user)]]
      user_char <- user_chars[as.character(user), ]
      all_eval <- all_evals[[as.character(user)]]
    }
    # --------------------------------------------------------------------------
    # --- Plots
    # --------------------------------------------------------------------------
    x <- seq(2, 5, l = 100)
    x2 <- 10^x
    cols <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c")
    
    if(save_stuff) jpeg(height = 500, width = 500, quality = 100, 
         file = paste0("figures/byuser/pred","_u", user, 
                       "_h", hour_shift, "_nt", nt, "_comp_all.jpeg"))
    plot(x, ecdf(all_eval[["s_bench1b"]]$avg_logdist)(x), 
         type = "l", lty = 1, col = cols[1], lwd = 2,
         xlim = c(2, 3.5),
         main = paste0("Evaluation of average log distances of user ", user),
         ylab = expression("ECDF of daily average log"[10]*" distance"),
         xlab = expression("log"[10]*" distance"))
    lines(x, ecdf(all_eval[["s_bench3b"]]$avg_logdist)(x), 
          col = cols[2], lty = 1, lwd = 2)
    lines(x, ecdf(all_eval[["s_cluster2"]]$avg_logdist)(x), 
          col = cols[4], lty = 1, lwd = 2)
    lines(x, ecdf(all_eval[["s_freq"]]$avg_logdist)(x), 
          col = cols[3], lty = 1, lwd = 2)
    lines(x, ecdf(all_eval[["handover"]]$handover_log_avg)(x), 
          col = 1      , lty = 1, lwd = 3)
    legend("bottomright", lwd = c(rep(2, 4), 3), col = c(cols, 1), 
           legend = c("All one cluster", "Workdays/Weekends", 
                      "Association Mining", "DAMOCLES", "Handover Benchmark"))
    if(save_stuff) dev.off()
    
    if(save_stuff) jpeg(height = 500, width = 500, quality = 100, 
         file = paste0("figures/byuser/pred","_u", user, 
                       "_h", hour_shift, "_nt", nt, "_comp_predonly.jpeg"))
    plot(x, ecdf(all_eval[["s_bench1b"]]$avg_logdist_pred_only)(x), 
         type = "l", lty = 1, col = cols[1], lwd = 2,
         xlim = c(2, 3.5),
         main = paste0("Evaluation of average log distances, ",
                       "pred only, of user ", user),
         ylab = expression("ECDF of daily average log"[10]*" distance"),
         xlab = expression("log"[10]*" distance"))
    lines(x, ecdf(all_eval[["s_bench3b"]]$avg_logdist_pred_only)(x), 
          col = cols[2], lty = 1, lwd = 2)
    lines(x, ecdf(all_eval[["s_cluster2"]]$avg_logdist_pred_only)(x), 
          col = cols[4], lty = 1, lwd = 2)
    lines(x, ecdf(all_eval[["s_freq"]]$avg_logdist_pred_only)(x), 
          col = cols[3], lty = 1, lwd = 2)
    lines(x, ecdf(all_eval[["handover"]]$handover_log_avg)(x), 
          col = 1      , lty = 1, lwd = 3)
    legend("bottomright", lwd = c(rep(2, 4), 3), col = c(cols, 1), 
           legend = c("All one cluster", "Workdays/Weekends", 
                      "Association Mining", "DAMOCLES", "Handover Benchmark"))
    if(save_stuff) dev.off()
    
    # --- Postlog Pictures
    
    if(save_stuff) {
      jpeg(height = 500, width = 500, quality = 100, 
           file = paste0("figures/byuser/pred","_u", user, 
                         "_h", hour_shift, "_nt", nt, "_comp_all_postlog.jpeg"))
    }
    plot(x2, ecdf(all_eval[["s_bench1b"]]$avg_dist)(x2), 
         type = "l", lty = 1, col = cols[1], lwd = 2,
         xlim = c(10^2, 10^3.5),
         main = paste0("Evaluation of average distances of user ", user),
         ylab = "ECDF of daily average distance",
         xlab = "Distance",
         log = "x")
    lines(x2, ecdf(all_eval[["s_bench3b"]]$avg_dist)(x2), 
          col = cols[2], lty = 1, lwd = 2)
    lines(x2, ecdf(all_eval[["s_cluster2"]]$avg_dist)(x2), 
          col = cols[4], lty = 1, lwd = 2)
    lines(x2, ecdf(all_eval[["s_freq"]]$avg_dist)(x2), 
          col = cols[3], lty = 1, lwd = 2)
    lines(x2, ecdf(all_eval[["handover"]]$handover_avg)(x2), 
          col = 1      , lty = 1, lwd = 3)
    legend("bottomright", lwd = c(rep(2, 4), 3), col = c(cols, 1), 
           legend = c("All one cluster", "Workdays/Weekends", 
                      "Association Mining", "DAMOCLES", "Handover Benchmark"))
    if(save_stuff) dev.off()
    
    if(save_stuff) {
      jpeg(height = 500, width = 500, quality = 100, 
           file = paste0("figures/byuser/pred","_u", user, 
                         "_h", hour_shift, "_nt", nt, 
                         "_comp_predonly_postlog.jpeg"))
    }
    plot(x2, ecdf(all_eval[["s_bench1b"]]$avg_dist_pred_only)(x2), 
         type = "l", lty = 1, col = cols[1], lwd = 2,
         xlim = c(10^2, 10^3.5),
         main = paste0("Evaluation of average distances, ",
                       "pred only, of user ", user),
         ylab = "ECDF of daily average distance",
         xlab = "Distance",
         log = "x")
    lines(x2, ecdf(all_eval[["s_bench3b"]]$avg_dist_pred_only)(x2), 
          col = cols[2], lty = 1, lwd = 2)
    lines(x2, ecdf(all_eval[["s_cluster2"]]$avg_dist_pred_only)(x2), 
          col = cols[4], lty = 1, lwd = 2)
    lines(x2, ecdf(all_eval[["s_freq"]]$avg_dist_pred_only)(x2), 
          col = cols[3], lty = 1, lwd = 2)
    lines(x2, ecdf(all_eval[["handover"]]$handover_avg)(x2), 
          col = 1      , lty = 1, lwd = 3)
    legend("bottomright", lwd = c(rep(2, 4), 3), col = c(cols, 1), 
           legend = c("All one cluster", "Workdays/Weekends", 
                      "Association Mining", "DAMOCLES", "Handover Benchmark"))
    if(save_stuff) dev.off()
    
    
    
    # places_info[[as.character(user)]] <- vis_gt(user_ = user, 
    #                                             eps_coords = 30, 
    #                                             minpts_coords = 4, 
    #                                             nt_ = nt,
    #                                             hour_shift = hour_shift,
    #                                             save_stuff = save_stuff)
    # if(save_stuff) save(places_info, file = paste0("results/places_info_", 
    #                                                hour_shift, "_", nt, ".rda"))
  },
  error = function(e){print(paste("Error at user", user, "\n", e))},
  warning = function(w){print(paste("Warning at user", user, "\n", w))}
  ))[3]), " ", Sys.time(), "\n"))
}  # User loop
rm(user_chars, all_evals, places_info, daily_infos)
}  # nt loop

