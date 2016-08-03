# This script gets some summary statistics

# ------------------------------------------------------------------------------
# --- Schulz' and Ranjan
# ------------------------------------------------------------------------------
# Gets Travelled distance and radius of Gyration for every user-day

# --- Cleaning & Initialisation
tryCatch(disconnect(), error = function(e){})
rm(list=ls())
source("00_defs_funcs.R")
source("loc_pred/02_b_get_raw_ground_truth.R")
source("loc_pred/02_get_segments_ground_truth.R")
library(parallel)
connect(user = "burkhard", 
        pw = scan("nosafe.txt", what = "character"), 
        dbname = "burkhard",
        host = "localhost", 
        port = 5432)

users <- sort(
  dbGetQuery(con, "select distinct uid from user_characteristics;")[, 1])
summary_stats_gps <- summary_stats_cdr <- summary_stats_gps_stops <- 
  summary_stats <- list()

hour_shift <- 3

# ------------------------------------------------------------------------------
# --- Function for getting stats out of a day
# ------------------------------------------------------------------------------
get_rog <- function(df, x_name, y_name, t_start, t_end, equal_weight = F){
  if(equal_weight){
    dt <- rep(1, nrow(df))
  }else{
    dt <- df[, t_end] - df[, t_start]
  }
  cent_x <- weighted.mean(df[, x_name], dt)
  cent_y <- weighted.mean(df[, y_name], dt)
  rog <- sqrt(weighted.mean((df[, x_name] - cent_x)^2 + 
                              (df[, y_name] - cent_y)^2, dt))
  return(rog)
}
get_dist <- function(df, x_name, y_name, ...){
  sum(sqrt(diff(df[, x_name])^2 + diff(df[, y_name])^2))
}

# ------------------------------------------------------------------------------
# --- Actual work
# ------------------------------------------------------------------------------
for(user in users){
  tryCatch({
  cat(paste(user, round(system.time({
  # ----------------------------------------------------------------------------
  # --- GPS/GSM
  # ----------------------------------------------------------------------------
  gps_gsm <- get_raw_gt_gps(user)
  summary_stats_gps[[as.character(user)]] <-
    data.frame(
      rog_gps = sapply(gps_gsm, get_rog, x_name = "x_mean", 
                       y_name = "y_mean", t_start = "f_start", t_end = "f_end"),
      dist_gps = sapply(gps_gsm, get_dist, x_name = "x_mean", 
                        y_name = "y_mean"),
      rog_gsm = sapply(gps_gsm, get_rog, x_name = "x_mast", y_name = "y_mast", 
                       t_start = "f_start", t_end = "f_end"),
      dist_gsm = sapply(gps_gsm, get_dist, x_name = "x_mast", y_name = "y_mast")
    )
  
  # ----------------------------------------------------------------------------
  # --- GPS stop segments
  # ----------------------------------------------------------------------------
  gps_seg <- get_segments_gt(user)
  summary_stats_gps_stops[[as.character(user)]] <- 
    data.frame(
      rog_gps_stop_e = sapply(gps_seg, get_rog, x_name = "x_mean", 
                       y_name = "y_mean", t_start = "f_start", t_end = "f_end",
                       equal_weight = T),
      rog_gps_stop_w = sapply(gps_seg, get_rog, x_name = "x_mean", 
                       y_name = "y_mean", t_start = "f_start", t_end = "f_end",
                       equal_weight = F),
      dist_gps_stop = sapply(gps_seg, get_dist, x_name = "x_mean", 
                        y_name = "y_mean")
    )
  
  # ----------------------------------------------------------------------------
  # --- CDR
  # ----------------------------------------------------------------------------
  cdrs <- dbGetQuery(con, paste0("
              SELECT 
              extract(doy   from a.t_start at time zone 'Europe/Tallinn' - interval '", hour_shift, " hours') as day,
              extract(epoch from AGE(a.t_start at time zone 'Europe/Tallinn' - interval '", hour_shift, " hours', 
              (a.t_start at time zone 'Europe/Tallinn' - interval '", hour_shift, " hours')::date)) / 3600/24 as frac,
              st_x(st_transform(b.geom, 3301)) as mast_x, 
              st_y(st_transform(b.geom, 3301)) as mast_y
              FROM all_cdr a LEFT JOIN masts b
              ON a.id_masts = b.id_masts
              WHERE a.uid = ", user, " AND
              a.id_masts is not NULL order by day, frac 
              ;"))
  cdr_list <- sapply(sort(unique(cdrs$day)), function(d_){
    one_day <- subset(cdrs, day == d_, drop = F)
    one_day$t_start_e <- 0
    one_day$t_end_e <- 1
    time_gaps <- c(0, one_day$frac[-1] - 0.5 * diff(one_day$frac), 1)
    one_day$t_start_w <- time_gaps[-length(time_gaps)]
    one_day$t_end_w <- time_gaps[-1]
    return(one_day)
  }, simplify = F)
  summary_stats_cdr[[as.character(user)]] <- 
    data.frame(
      rog_cdr_e = sapply(cdr_list, get_rog, "mast_x", "mast_y", "t_start_e", 
                         "t_end_e"),
      rog_cdr_w = sapply(cdr_list, get_rog, "mast_x", "mast_y", "t_start_w", 
                         "t_end_w"),
      dist_cdr = sapply(cdr_list, get_dist, "mast_x", "mast_y"),
      no_calls = sapply(cdr_list, function(df_) nrow(df_)),
      timespans_w = sapply(cdr_list, function(df_){
        weighted.mean(df_[, "t_end_w"] - df_[, "t_start_w"], 
                      df_[, "t_end_w"] - df_[, "t_start_w"])
      })
    )
  days_of_all <- rownames(summary_stats_gps[[as.character(user)]]) %>%
    intersect(rownames(summary_stats_cdr[[as.character(user)]])) %>%
    intersect(rownames(summary_stats_gps_stops[[as.character(user)]]))
                            
  
  summary_stats[[as.character(user)]] <- 
    cbind(summary_stats_gps[[as.character(user)]][days_of_all, ],
          summary_stats_cdr[[as.character(user)]][days_of_all, ],
          summary_stats_gps_stops[[as.character(user)]][days_of_all, ])
  
  })[3]), " ", "\n"))
  flush.console()
  },
  error = function(e){print(paste("Error at user", user, "\n", e))},
  warning = function(w){print(paste("Warning at user", user, "\n", w))})
}  # End user loop

# ------------------------------------------------------------------------------
# --- Visualisation
# ------------------------------------------------------------------------------
myjitter <- function(v, fraction = 0.02){
  a <- (max(v, na.rm=T) - min(v, na.rm = T)) * fraction
  v + runif(length(v), -a/2, a/2)
}
save_imgs <- T
i <- 8598
i <- i+1
set.seed(i)
ex_user <- "116"
n1 <- c("rog_gps", "rog_gsm", "rog_gps_stop_w", "rog_gps_stop_e", 
        "rog_cdr_w", "rog_cdr_e")
n2 <- c("dist_gps", "dist_gsm", "dist_gps_stop", "dist_cdr")
everything <- subset(do.call(what = rbind, summary_stats), rog_gps_stop_e > 1)
apply(everything[, c(n1, n2)], 2, mean) / 10^4

# used_data <- log10(as.matrix(summary_stats[[ex_user]]))
used_data_p <- log10(as.matrix(everything[sample(nrow(everything), 600, 
                                               replace = F), ]))
used_data <- matrix(pmax(0, used_data_p), nrow = nrow(used_data_p)) %>%
  apply(2, myjitter, fraction = 0.15)
colnames(used_data) <- colnames(used_data_p)
used_data[, c("timespans_w", "no_calls")] <- 10^used_data_p[, c("timespans_w", "no_calls")]

col_scale1 <- c("#fdae61", "#abd9e9", "#2c7bb6")
col_scale2 <- c("#dfc27d", "#80cdc1", "#018571")
t1 <- used_data[, "timespans_w"]
c1 <- col_scale1[2 + (t1 < quantile(t1, 0.25)) - (t1 > quantile(t1, 0.75))]
t2 <- used_data[, "no_calls"]
c2 <- col_scale2[2 + (t2 > quantile(t2, 0.75)) - (t2 < quantile(t2, 0.25))]
cat(paste("\nSoll:\n",
          paste(round(log10(quantile(everything$timespans_w, c(0.25, 0.75))), 2), collapse = " "), "\n",
          paste(round(log10(quantile(everything$no_calls, c(0.25, 0.75))), 2), collapse = " "), "\n",
          "Ist:\n",
          paste(round(quantile(t1, c(0.25, 0.75)), 2), collapse = " "), "\n",
          paste(round(quantile(t2, c(0.25, 0.75)), 2), collapse = " "), "\n"))
flush.console()

lower_part <- function(x,y, c1, ...){
  points(x,y, col = c1, ...)
  abline(a = 0,b = 1)
}
upper_part <- function(x,y, c2, ...){
  points(x,y, col = c2, ...)
  abline(a = 0,b = 1)
}

if(save_imgs) jpeg("figures/rog.jpeg", quality = 100, height = 625, width = 600)
pairs(used_data[, n1], lower.panel = lower_part, c1 = c1,
      upper.panel = upper_part, c2 = c2, asp = 1, cex = 0.3, lwd = 2,
      xlim = c(0, 6), ylim = c(0, 6),
      main = "Pairs Plot of Different Radii of Gyration")
if(save_imgs) dev.off()

if(save_imgs) jpeg("figures/dists.jpeg", quality = 100, height = 625, width = 600)
pairs(used_data[, n2], lower.panel = lower_part, c1 = c1,
      upper.panel = upper_part, c2 = c2, asp = 1, cex = 0.3, lwd = 2,
      xlim = c(0, 6.5), ylim = c(0, 6.5),
      main = "Pairs Plot of Different Measures of Travelled Distance")
if(save_imgs) dev.off()


