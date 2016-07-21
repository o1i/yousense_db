get_raw_gt_gps <- function(user_, hour_shift = 3){
  q <- paste0("
              SELECT uid, 
              t_start at time zone 'GMT' as t_start, 
              t_end at time zone 'GMT' as t_end, 
              st_x(st_transform(geom_gps,  3301)) AS x_mean, 
              st_y(st_transform(geom_gps,  3301)) AS y_mean, 
              st_x(st_transform(geom_mast, 3301)) AS x_mast, 
              st_y(st_transform(geom_mast, 3301)) AS y_mast, 
              extract(DOY FROM t_start at time zone 'Europe/Tallinn' - INTERVAL '", 
              hour_shift, " hours')::smallint as day,

              extract(DOY FROM t_end at time zone 'Europe/Tallinn' - INTERVAL '", 
              hour_shift, " hours')::smallint AS  day_next,

              extract(epoch from AGE(t_start at time zone 'Europe/Tallinn' - interval '", 
              hour_shift, " hours', 
              (t_start at time zone 'Europe/Tallinn' - interval '", 
              hour_shift, " hours')::date)) / 3600/24 as f_start,

              extract(epoch from AGE(t_end at time zone 'Europe/Tallinn' - interval '", 
              hour_shift, " hours', 
              (t_end at time zone 'Europe/Tallinn' - interval '", 
              hour_shift, " hours')::date)) / 3600/24 as f_end

              FROM 
              gps_gsm
              WHERE 
              uid = ", user, " AND
              flag_problem = 1
              order by t_start, t_end
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

  gt_by_day <- gt_by_day[sapply(gt_by_day, nrow) > 0]
  return(gt_by_day)
}