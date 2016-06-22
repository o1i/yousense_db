get_segments_gt <- function(user, hour_shift = 3, include_moves = F){
  # Gets the ground truth of a user as determined by segments
  # Requires the 
  # user (numeric) as well as the
  # hour_shift, number indicating when days start. 
  
  library(lubridate)
  q <- paste0("
              SELECT uid, t_start, t_end, 
              st_x(st_transform(geom_mean, 3301)) AS x_mean, 
              st_y(st_transform(geom_mean, 3301)) AS y_mean, 
              extract(DOY FROM t_start - INTERVAL '", 
                hour_shift, " hours')::smallint as day,
              extract(DOY FROM t_end - INTERVAL '", 
                hour_shift, " hours')::smallint AS  day_next,
              valid_start, valid_end, stop
              FROM 
              segments
              WHERE 
              uid = ", user, "
              ;")
  ground_truth <- dbGetQuery(con,q)
  # The timestamps have to be converted into fractions since the shifted 
  #   midnight
  ground_truth$f_start <- day_frac(ground_truth$t_start - 
                                     as.duration(hour_shift * 3600))
  ground_truth$f_end   <- day_frac(ground_truth$t_end   - 
                                     as.duration(hour_shift * 3600))
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
    lapply(gt_by_day, function(day_){
      subset(day_, stop)
    })
  }
  return(gt_by_day)
}