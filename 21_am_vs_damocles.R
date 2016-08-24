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

force_fit <- T
save_stuff <- T
eval_stuff <- T

users <- 1:50

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
    if(ii <= 10){
      user_data[[ii]] <- create_user_realisations(n_ = 200, 
                                                  n_routines_ = 6, 
                                                  n_masts_ = 10,
                                                  masts_ = masts, 
                                                  nt_ = 24, 
                                                  hour_shift_ = hour_shift,
                                                  mean1_ = 3,
                                                  mean2_ = 3, 
                                                  lim_ = 1000,
                                                  min_ = 0, 
                                                  max_ = 4,  
                                                  equal_ = 0.3, 
                                                  scale_ = 5)
    }else if(ii <= 20){
      user_data[[ii]] <- create_user_realisations(n_ = 200, 
                                                  n_routines_ = 3, 
                                                  n_masts_ = 10,
                                                  masts_ = masts, 
                                                  nt_ = 24, 
                                                  hour_shift_ = hour_shift,
                                                  mean1_ = 3,
                                                  mean2_ = 3, 
                                                  lim_ = 1000,
                                                  min_ = 0, 
                                                  max_ = 4,  
                                                  equal_ = 0.3, 
                                                  scale_ = 7)
    }else if(ii <= 30){
      user_data[[ii]] <- create_user_realisations(n_ = 200, 
                                                  n_routines_ = 6, 
                                                  n_masts_ = 10,
                                                  masts_ = masts, 
                                                  nt_ = 24, 
                                                  hour_shift_ = hour_shift,
                                                  mean1_ = 5,
                                                  mean2_ = 3, 
                                                  lim_ = 1000,
                                                  min_ = 0, 
                                                  max_ = 6,  
                                                  equal_ = 0.3, 
                                                  scale_ = 6)
    }else if(ii <= 40){
      user_data[[ii]] <- create_user_realisations(n_ = 200, 
                                                  n_routines_ = 10, 
                                                  n_masts_ = 6,
                                                  masts_ = masts, 
                                                  nt_ = 24, 
                                                  hour_shift_ = hour_shift,
                                                  mean1_ = 3,
                                                  mean2_ = 4, 
                                                  lim_ = 1000,
                                                  min_ = 0, 
                                                  max_ = 6,  
                                                  equal_ = 0.3, 
                                                  scale_ = 5)
    }else{
      user_data[[ii]] <- create_user_realisations(n_ = 200, 
                                                  n_routines_ = 3, 
                                                  n_masts_ = 3,
                                                  masts_ = masts, 
                                                  nt_ = 24, 
                                                  hour_shift_ = hour_shift,
                                                  mean1_ = 2,
                                                  mean2_ = 4, 
                                                  lim_ = 2000,
                                                  min_ = 1, 
                                                  max_ = 3, 
                                                  equal_ = 0.3, 
                                                  scale_ = 4)
    }
    
    for(jj in 1:length(user_data[[ii]][["gt_formatted"]])){
      user_data[[ii]][["gt_formatted"]][[jj]]$uid <- ii
    }
  }
  save(user_data, file = filename)
}else{
  load(filename)
}
display_routines(user_data[[48]]$routines)


for(duration in c(30, 60, 100, 200)){
user_chars <- initialise("user_chars", paste0("simulation/user_chars_",
                                              hour_shift, "_", nt,  
                                              "_d", duration,
                                              ".rda"), get_empty_user_char())
all_evals <-  initialise("all_evals", paste0("simulation/all_evals_",
                                             hour_shift, "_", nt,
                                             "_d", duration,
                                             ".rda"), list())
# places_info <-  initialise("places_info", paste0("simulation/places_info_", 
#                                                  hour_shift, "_", nt, 
#                                   ".rda"), list())
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
                                    dirpath_ = "simulation/figures/byuser/")
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
    }else{
      load(paste0("simulation/daily_infos_", hour_shift, "_", nt,
                  "_d", duration, ".rda"))
      load(paste0("simulation/user_chars_",  hour_shift, "_", nt, 
                  "_d", duration, ".rda"))
      load(paste0("simulation/all_evals_",   hour_shift, "_", nt, 
                  "_d", duration, ".rda"))
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
