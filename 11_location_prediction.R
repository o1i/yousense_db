# This script applies the other functions in the folder in order to get
#   predictions.
rm(list=ls())

# --- Set Parameters -----------------------------------------------------------
nt <- 6  # Number of parts to split a day into.
library(reshape2)
library(plyr)
source("00_defs_funcs.R")
lapply(list.files("loc_pred", full.names = T), source)
library(leaflet)

connect(user = "burkhard", 
        pw = scan("nosafe.txt", what = "character"), 
        dbname = "burkhard",
        host = "localhost", 
        port = 5432)

results <- list()
user_chars <- get_empty_user_char()
daily_losses <- list()
for(user in sort(
  dbGetQuery(con, "select distinct uid from user_characteristics;")[, 1])){
  print(user)

  tryCatch({
    # --- Get user days --------------------------------------------------------
    temp <- get_user_days(user, nt_ = nt, level_ = "CDR", hour_shift = 2)
    seen_masts <- temp[["seen_masts"]]
    days <- temp[["days"]]
    rm(temp)
    
    # --- Create Predictions ---------------------------------------------------
    p_bench1 <- bench1(days)
    p_bench2 <- bench2(days)
    p_bench3 <- bench3(days)
    p_cluster <- pred_cluster(days_ = days, nt_ = nt, num_clus = 5)
    # p_cluster_v1 <- pred_cluster_v1(days_ = days)
    
    # --- Get Ground Truth data ------------------------------------------------
    gt <- get_segments_gt(user, hour_shift = 3, include_moves = F)
    
    # --- Evaluate the three predictions ---------------------------------------
    d_bench1 <- eval_pred(ground_truth_ = gt, prediction_ = p_bench1, 
                          masts_ = "seen_masts", log_ = T)
    d_bench2 <- eval_pred(ground_truth_ = gt, prediction_ = p_bench2, 
                          masts_ = "seen_masts", log_ = T)
    d_bench3 <- eval_pred(ground_truth_ = gt, prediction_ = p_bench3, 
                          masts_ = "seen_masts", log_ = T)
    d_cluster <- eval_pred(ground_truth_ = gt, prediction_ = p_cluster, 
                           masts_ = "seen_masts", log_ = T)
    # d_cluster_v1 <- eval_pred(ground_truth_ = gt, prediction_ = p_cluster_v1, 
    #                            masts_ = "seen_masts", log_ = T)
    
    
    # --- store Results --------------------------------------------------------
    # --- numbers
    user_chars <- rbind(user_chars, user_char(days, gt, user))
    save(user_chars, file = "results/user_chars.rda")
    daily_losses[[as.character(user)]] <- list(d_bench1 = d_bench1,
                                             d_bench2 = d_bench2,
                                             d_bench3 = d_bench3,
                                             d_cluster = d_cluster)
    save(daily_losses, file = "results/daily_losses.rda")
    
    # --- image
    x <- seq(2, 5, l = 100)
    jpeg(height = 500, width = 500, quality = 100, 
         file = paste0("figures/pred_comp_u", user, ".jpeg"))
    plot(x, ecdf(d_bench1)(x), type = "l", main = paste0("Comp, user ", user),
         ylab = "ecdf of days below a certain avg error",
         xlab = "log10 of distance")
    lines(x, ecdf(d_bench2)(x), col = 2)
    lines(x, ecdf(d_bench3)(x), col = 3) 
    lines(x, ecdf(d_cluster)(x), col = 4)
    # lines(x, ecdf(d_cluster_v1)(x), col = 5)
    dev.off()
    results[[as.character(user)]] <- list(d_bench1 = d_bench1,
                                          d_bench2 = d_bench2,
                                          d_bench3 = d_bench3,
                                          d_cluster = d_cluster)
  },
  error = function(e){print(paste("Error at user", user))},
  warning = function(w){print(paste("Warning at user", user))}
  )
}

