# This script applies the other functions in the folder in order to get
#   predictions.
tryCatch(disconnect(), error = function(e){})
rm(list=ls())

# ------------------------------------------------------------------------------
# --- Set Parameters 
# ------------------------------------------------------------------------------

nt <- 24  # Number of parts to split a day into.
hour_shift <- 3
library(reshape2)
library(plyr)
source("00_defs_funcs.R")
source("22_simulate_user_days.R")
source("23_evaluate_user_days.R")
lapply(list.files("loc_pred", full.names = T), source)
library(leaflet)
library(parallel)
library(dbscan)

connect(pw = scan("nosafe.txt", what = "character"))

initialise <- function(obj_name, filename, init, force_new_ = F){
  if(file.exists(filename)){
    if(force_new_){
      file.remove(filename)
      init
    }else{
      load(filename)
      print(paste(filename, "loaded"))
      return(get(obj_name))
    }
  }else{
    init
  }
}

force_fit <- T
save_stuff <- T
eval_stuff <- T
force_new <- T

block_size <- 20
num_blocks <- 16
users <- 1:(block_size * num_blocks)

# ------------------------------------------------------------------------------
# --- Create the data
# ------------------------------------------------------------------------------

user_data <- as.list(users)
set.seed(1337)
masts <- get_masts_of_area(x_ = 26.7231014, 
                           y_ = 58.3784483, 
                           buffer_ = 10000)
filename = "simulation/user_data.rda"


if(!file.exists(filename) | force_fit){
  for(ii in users){
    print(paste("User", ii))

    n_routines <- 2 + block_number(block_number(user, block_size), 1, 2) * 2
    n_masts    <- 5 + block_number(block_number(user, block_size), 2, 2) * 10
    equal      <- 0 + block_number(block_number(user, block_size), 4, 2) * 0.4
    scale      <- 3 + block_number(block_number(user, block_size), 8, 2) * 6
    
    user_data[[ii]] <- create_user_realisations(n_ = 200, 
                                                n_routines_ = n_routines, 
                                                n_masts_ = n_masts,
                                                masts_ = masts, 
                                                nt_ = 24, 
                                                hour_shift_ = hour_shift,
                                                mean1_ = 3,
                                                mean2_ = 4, 
                                                lim_ = 500,
                                                min_ = 3, 
                                                max_ = 7,  
                                                equal_ = equal, 
                                                scale_ = scale)
    
    for(jj in 1:length(user_data[[ii]][["gt_formatted"]])){
      user_data[[ii]][["gt_formatted"]][[jj]]$uid <- ii
    }
  }
  save(user_data, file = filename)
}else{
  load(filename)
}
display_routines(routines_ = user_data[[42]]$routines, 
                 nt_ = 24,
                 masts_ = masts)


for(duration in c(200, 60)){
user_chars <- initialise("user_chars", paste0("simulation/user_chars_",
                                              hour_shift, "_", nt,  
                                              "_d", duration,
                                              ".rda"), get_empty_user_char())
all_evals <-  initialise("all_evals", paste0("simulation/all_evals_",
                                             hour_shift, "_", nt,
                                             "_d", duration,
                                             ".rda"), list())
all_stats <-  initialise("all_stats", paste0("simulation/all_stats_",
                                             hour_shift, "_", nt,
                                             "_d", duration,
                                             ".rda"), list())
daily_infos <- initialise("daily_infos", paste0("simulation/daily_infos_",
                                                hour_shift, "_", nt, 
                                                "_d", duration,
                                                ".rda"), list())
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

    seen_masts <- user_data[[user]][["seen_masts"]]
    days <- user_data[[user]][["obs"]][1:duration, ]
    
    # --------------------------------------------------------------------------
    # --- Create Predictions 
    # --------------------------------------------------------------------------
    days <- days[which.max(as.numeric(rownames(days)) < 300):nrow(days), ]
    filename <- paste0("simulation/fits/fits_", hour_shift, "_", nt,
                       "_", user, ".rda")
    if(force_fit | !file.exists(filename)){
      p_cluster2 <- pred_cluster_v3(days_ = days, 
                                    nt_ = nt, 
                                    min_pts = 3, 
                                    seen_masts_ = seen_masts, 
                                    scale_ = 1, 
                                    pen_ = 0.5, 
                                    thresh_ = 0.5,
                                    save_stuff = save_stuff, 
                                    eps_ = 0.05, 
                                    minpts_ = 4,
                                    dirpath_ = 
                                      paste0("simulation/figures/byuser/d", 
                                             duration, "_"))
      # p_cluster_v1 <- pred_cluster_v1(days_ = days)
      p_freq <- pred_freq(days)
      if(save_stuff)  save("p_cluster2", "p_freq", file = filename)
    }else{
      load(filename)
    }
    
    # --------------------------------------------------------------------------
    # --- Get Ground Truth data 
    # --------------------------------------------------------------------------
    #gt <- get_segments_gt(user, hour_shift = hour_shift, include_moves = F)
    gt_raw <- user_data[[user]]$gt_formatted[1:duration]
    # gt_masts <- get_segments_gt_masts(user, seen_masts_ = seen_masts, 
    #                                   nt_ = nt, hour_shift = hour_shift)
    # gt_masts <- subset(gt_masts, names(gt_masts) %in% names(gt_raw))
    
    # --------------------------------------------------------------------------
    # --- Evaluate the predictions
    # --------------------------------------------------------------------------
    if(eval_stuff){
      # --- Evaluation against raw gps
      eval_dist <- lapply(list(p_cluster2, p_freq), 
                          function(pred){
                            eval_pred(
                              ground_truth_ = gt_raw,
                              prediction_ = pred,
                              func = day_comp_1,
                              masts_ = "seen_masts", 
                              give_days = T,
                              days_ = days)
                          })
      names(eval_dist) <- c("s_cluster2", "s_freq")
      
      # --- get TD and RoG
      lcd <- apply(user_data[[user]]$gt[1:duration, ] == p_cluster2, 1, mean)
      lcf <- apply(user_data[[user]]$gt[1:duration, ] == p_freq,     1, mean)
      stats <- data.frame(uid = user,
                          td_damocles = get_dist_travelled(p_cluster2, masts),
                          rog_damocles = get_rog(p_cluster2, masts),
                          td_freq = get_dist_travelled(p_freq, masts),
                          rog_freq = get_rog(p_freq, masts),
                          td_gt = get_dist_travelled(user_data[[ii]]$gt[
                            1:duration, ], masts),
                          rog_gt = get_rog(user_data[[ii]]$gt[1:duration, ], 
                                           masts),
                          label_correctness_dam = lcd,
                          label_correctness_freq = lcf,
                          recon_err_dam  = eval_dist[["s_cluster2"]]$avg_dist,
                          recon_err_freq = eval_dist[["s_freq"]]$avg_dist,
                          num_cdr = apply(user_data[[user]]$obs, 
                                           1, function(v_){sum(!is.na(v_))})
                          )
      
      # 
      # # --- Maximally possible accuracy
      # eval_handover <- day_comp_handover(gt_ = gt_raw)
      # 
      # 
      # # --- Evaluation on the label level
      # eval_labels <- lapply(list(p_bench1, p_bench1b, p_bench2, p_bench3, 
      #                            p_bench3b, p_cluster, 
      #                            p_cluster2, p_freq, p_freq_h), function(pred){
      #                              eval_pred(ground_truth_ = gt_masts,
      #                                        prediction_ = pred,
      #                                        func = day_comp_2,
      #                                        days_ = days,
      #                                        give_days = F
      #                              )
      #                            }) 
      # names(eval_labels) <- c("s_bench1", "s_bench1b", "s_bench2", "s_bench3", 
      #                         "s_bench3b", "s_cluster",
      #                         "s_cluster2", "s_freq", "s_freq_h")
      
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
      all_eval <- c(sapply(names(eval_dist), function(n_){
        cbind(eval_dist[[n_]])
      }, simplify = F), 
      list(eval_days = eval_dist[[1]]$eval_days))
      
      # ------------------------------------------------------------------------
      # --- store Results
      # ------------------------------------------------------------------------
      # file should be called daily_infos_... change next time.
      daily_infos[[as.character(user)]] <- daily_info
      if(save_stuff) save(daily_infos, file = paste0("simulation/daily_infos_", 
                                                     hour_shift,
                                                     "_", nt,  "_d", duration,
                                                     ".rda"))
      # --- numbers
      user_chars[as.character(user), ] <- user_char(days, gt_raw, user)
      if(save_stuff) save(user_chars, file = paste0("simulation/user_chars_", 
                                                    hour_shift,
                                                    "_", nt,  "_d", duration,
                                                    ".rda"))
      all_evals[[as.character(user)]] <- all_eval
      if(save_stuff) save(all_evals, 
                          file = paste0("simulation/all_evals_", 
                                        hour_shift, "_", nt,  "_d", duration,
                                        ".rda"))
      
      all_stats[[as.character(user)]] <- stats
      if(save_stuff) save(all_stats, 
                          file = paste0("simulation/all_stats_", 
                                        hour_shift, "_", nt,  "_d", duration,
                                        ".rda"))
    }else{
      load(paste0("simulation/daily_infos_", hour_shift, "_", nt,
                  "_d", duration, ".rda"))
      load(paste0("simulation/user_chars_",  hour_shift, "_", nt, 
                  "_d", duration, ".rda"))
      load(paste0("simulation/all_evals_",   hour_shift, "_", nt, 
                  "_d", duration, ".rda"))
      load(paste0("simulation/all_stats_",   hour_shift, "_", nt, 
                  "_d", duration, ".rda"))
      daily_info <- daily_infos[[as.character(user)]]
      user_char <- user_chars[as.character(user), ]
      all_eval <- all_evals[[as.character(user)]]
      stats <- all_stats[[as.character(user)]]
    }
    # --------------------------------------------------------------------------
    # --- Plots
    # --------------------------------------------------------------------------
    x <- seq(2, 5, l = 100)
    x2 <- 10^x
    cols <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c")
    
    # --- Postlog Pictures
    # if(save_stuff) {
    #   jpeg(height = 500, width = 500, quality = 100, 
    #        file = paste0("simulation/figures/byuser/pred","_u", user, 
    #                      "_h", hour_shift, "_nt", nt,  "_d", duration,
    #                      "_comp_all_postlog.jpeg"))
    # }
    # plot(x2, ecdf(all_eval[["s_cluster2"]]$avg_dist)(x2), 
    #      type = "l", 
    #      lty = 1, col = cols[4], lwd = 2,
    #      xlim = c(10^2, 10^3.5),
    #      main = paste0("Evaluation of average distances of user ", user),
    #      ylab = "ECDF of daily average distance",
    #      xlab = "Distance",
    #      log = "x")
    # lines(x2, ecdf(all_eval[["s_freq"]]$avg_dist)(x2), 
    #       col = cols[2], lty = 1, lwd = 2)
    # legend("bottomright", col = c(cols[4], cols[2]), lwd = 2, 
    #        legend = c("DAMOCLES", "Association Mining"))
    # if(save_stuff) dev.off()
    if(save_stuff) {
      jpeg(height = 500, width = 500, quality = 100, 
           file = paste0("simulation/figures/byuser/pred","_u", user, 
                         "_h", hour_shift, "_nt", nt, "_d", duration,
                         "_comp_predonly_postlog.jpeg"))
    }
    plot(x2, ecdf(all_eval[["s_cluster2"]]$avg_dist_pred_only)(x2), 
         type = "l",
         lty = 1, col = cols[4], lwd = 2,
         xlim = c(10^2, 10^3.5),
         main = paste0("Evaluation of average distances, ",
                       "pred only, of user ", user),
         ylab = "ECDF of daily average distance",
         xlab = "Distance",
         log = "x")
    lines(x2, ecdf(all_eval[["s_freq"]]$avg_dist_pred_only)(x2), 
          col = cols[2], lty = 1, lwd = 2)
    legend("bottomright", col = c(cols[4], cols[2]), lwd = 2, 
           legend = c("DAMOCLES", "Association Mining"))
    if(save_stuff) dev.off()
  },
  error = function(e){print(paste("Error at user", user, "\n", e))},
  warning = function(w){print(paste("Warning at user", user, "\n", w))}
  ))[3]), " ", Sys.time(), "\n"))
}  # User loop
}  # duration loop
rm(user_chars, all_evals, daily_infos)

# ------------------------------------------------------------------------------
# --- Evaluaiton
# -----------------------------------------------------------------------------

duration <- 200
load(paste0("simulation/all_stats_",   hour_shift, "_", nt, 
            "_d", duration, ".rda"))


stats_frequent <- t(sapply(names(all_stats), function(n){
  df_ <- subset(all_stats[[n]], daily_infos[[n]]$count >= 6)
  c(mean(df_$td_damocles / df_$td_gt), mean(df_$td_freq / df_$td_gt), 
    mean(df_$rog_damocles / df_$rog_gt), mean(df_$rog_freq / df_$rog_gt))
}))

stats_rare <- t(sapply(names(all_stats), function(n){
  df_ <- subset(all_stats[[n]], daily_infos[[n]]$count <= 3)
  c(mean(df_$td_damocles / df_$td_gt), mean(df_$td_freq / df_$td_gt), 
    mean(df_$rog_damocles / df_$rog_gt), mean(df_$rog_freq / df_$rog_gt))
}))

stats_mid <- t(sapply(names(all_stats), function(n){
  df_ <- subset(all_stats[[n]], 
                daily_infos[[n]]$count > 3 & daily_infos[[n]]$count < 6)
  c(mean(df_$td_damocles / df_$td_gt), mean(df_$td_freq / df_$td_gt), 
    mean(df_$rog_damocles / df_$rog_gt), mean(df_$rog_freq / df_$rog_gt))
}))

stats_collection <- t(sapply(all_stats, function(df_){
  c(mean(df_$td_damocles / df_$td_gt), mean(df_$td_freq / df_$td_gt), 
    mean(df_$rog_damocles / df_$rog_gt), mean(df_$rog_freq / df_$rog_gt))
}))

test_df <- stats_rare
par(mfrow = c(2, 1))
  plot(log10(test_df[, 1]), col = 3, lwd = 2, type = "p", 
       main = "Distance", ylab = "Proportion")
points(log10(test_df[, 2]), col = 2, lwd = 2)
abline(v = 1:10 * 10 + 0.5, h = 0)
  plot(log10(test_df[, 3]), col = 3, lwd = 2, pch = 2, type = "p", 
       main = "Rog", ylab = "Proportion")
points(log10(test_df[, 4]), col = 2, lwd = 2, pch = 2)
abline(v = 1:10 * 10 + 0.5, h =0)


