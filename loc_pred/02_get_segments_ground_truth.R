get_segments_gt <- function(user, hour_shift = 3, include_moves = F){
  # Gets the ground truth of a user as determined by segments
  # Requires the 
  # user (numeric) as well as the
  # hour_shift, number indicating when days start. 
  
  library(lubridate)
  # TIME IN GMT! (and displayed according to system preferences)
  q <- paste0("
              SELECT uid, 
              t_start at time zone 'GMT' as t_start, 
              t_end at time zone 'GMT' as t_end, 
              st_x(st_transform(geom_mean, 3301)) AS x_mean, 
              st_y(st_transform(geom_mean, 3301)) AS y_mean, 
              extract(DOY FROM t_start at time zone 'Europe/Tallinn' - INTERVAL '", 
                hour_shift, " hours')::smallint as day,
              extract(DOY FROM t_end at time zone 'Europe/Tallinn' - INTERVAL '", 
                hour_shift, " hours')::smallint AS  day_next,
              extract(epoch from AGE(t_start at time zone 'Europe/Tallinn' - interval '", hour_shift, " hours', 
              (t_start at time zone 'Europe/Tallinn' - interval '", hour_shift, " hours')::date)) / 3600/24 as f_start,
              extract(epoch from AGE(t_end at time zone 'Europe/Tallinn' - interval '", hour_shift, " hours', 
              (t_end at time zone 'Europe/Tallinn' - interval '", hour_shift, " hours')::date)) / 3600/24 as f_end,
              valid_start, valid_end, stop
              FROM 
              segments
              WHERE 
              uid = ", user, "
              ;")
  ground_truth <- dbGetQuery(con,q)
  # The timestamps have to be converted into fractions since the shifted 
  #   midnight

  # Get a list of GT-days
  gt_by_day <- sapply(min(ground_truth$day):max(ground_truth$day_next), 
                      simplify = F,
                      FUN = function(day_){
                        subset(ground_truth, day <= day_ & day_next >= day_)
                      })
  names(gt_by_day) <- min(ground_truth$day):max(ground_truth$day_next)
  gt_by_day <- gt_by_day[which(sapply(gt_by_day, nrow) != 0)]
  
  # In case the first or last segments do not start or end exactly at
  #   "midnight", prune them.
  for(i in 1:length(gt_by_day)){
    n <- nrow(gt_by_day[[i]])
    if(gt_by_day[[i]]$day[1] != gt_by_day[[i]]$day_next[1]){
      gt_by_day[[i]][1, "f_start"] <- 0
    } 
    if(gt_by_day[[i]]$day[n] != gt_by_day[[i]]$day_next[n]){
      gt_by_day[[i]][n, "f_end"] <- 1
    } 
  }
  if(!include_moves){
    gt_by_day <- lapply(gt_by_day, function(day_){
      subset(day_, stop)
    })
  }
  gt_by_day <- gt_by_day[sapply(gt_by_day, nrow) > 0]
  return(gt_by_day)
}

# ------------------------------------------------------------------------------
# --- Mast-Level Ground Truth --------------------------------------------------
# ------------------------------------------------------------------------------
get_segments_gt_masts <- function(user, seen_masts_, nt_ = 6, hour_shift = 2){
  q <- paste0("
  SELECT 
  t_start, t_end,
  extract(epoch from AGE(t_start at time zone 'Europe/Tallinn' - interval '", hour_shift, " hours', 
              (t_start at time zone 'Europe/Tallinn' - interval '", hour_shift, " hours')::date)) / 3600/24 as frac_start,
  extract(epoch from AGE(t_end at time zone 'Europe/Tallinn' - interval '", hour_shift, " hours', 
              (t_end at time zone 'Europe/Tallinn' - interval '", hour_shift, " hours')::date)) / 3600/24 as frac_end, 
  flag_problem, id_masts, 
  extract(DOY FROM t_start at time zone 'Europe/Tallinn' - INTERVAL '", 
  hour_shift, " hours')::smallint as day_start,
  extract(DOY FROM t_end at time zone 'Europe/Tallinn' - INTERVAL '", 
  hour_shift, " hours')::smallint AS  day_end
  FROM gps_gsm
  WHERE uid = ", user, "
  ORDER BY t_start
  ;")
  all_conn <- dbGetQuery(con, q)
  all_conn$cluster <- seen_masts_[as.character(all_conn$id_masts), "cluster"]
  all_conn$id_masts[all_conn$cluster > 0 & !is.na(all_conn$cluster)] <- 
    all_conn$cluster[all_conn$cluster > 0 & !is.na(all_conn$cluster)]
  breaks <- seq(0, 1, l = nt_ + 1)
  gt_by_day <- lapply(min(all_conn$day_start):max(all_conn$day_end), function(d_){
    df <- subset(all_conn, day_start <= d_ & day_end >= d_, drop = F)
    if(nrow(df) > 0){
      df[df$day_start < d_, "frac_start"] <- 0
      df[df$day_end   > d_, "frac_end"] <- 1
      aggs <- aggregate(t(apply(df[, c("frac_start", "frac_end")], 1, function(v){
        pmax(breaks[-length(breaks)], pmin(v[2], breaks[-1])) - 
          pmax(breaks[-length(breaks)], pmin(v[1], breaks[-1]))
      })), by = list(df$id_masts), FUN = sum)
    }else{
      aggs <- matrix(0, nrow = 0, ncol = nt_ + 1)
    }
    colnames(aggs) <- c("id_masts", (0:(ncol(aggs) - 2) * (24/nt_) + hour_shift) %% 24)
    return(aggs)
  })
  names(gt_by_day) <- min(all_conn$day_start):max(all_conn$day_end)
  gt_by_day[sapply(gt_by_day, nrow)==0] <- NULL
  return(gt_by_day)
}






