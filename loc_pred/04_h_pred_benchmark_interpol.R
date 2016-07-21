bench4 <- function(user_ = 1, nt2 = 60 * 24, masts_ = "seen_masts"){
  q <- paste0("
              SELECT uid, 
              t_start at time zone 'GMT' as t_start, 
              id_masts,
              extract(DOY FROM t_start at time zone 'Europe/Tallinn' - INTERVAL '", 
              hour_shift, " hours')::smallint as day,

              extract(epoch from AGE(t_start at time zone 'Europe/Tallinn' - interval '", 
              hour_shift, " hours', 
              (t_start at time zone 'Europe/Tallinn' - interval '", 
              hour_shift, " hours')::date)) / 3600/24 as f_start

              FROM 
              all_cdr
              WHERE 
              uid = ", user, " AND
              id_masts IS NOT NULL
              order by t_start
              ;")
  ground_truth <- dbGetQuery(con,q)
  # The timestamps have to be converted into fractions since the shifted 
  #   midnight
  
  ground_truth[, c("x", "y")] <- 
    get(masts_)[as.character(ground_truth$id_masts), c("mast_x", "mast_y")]
  
  # Get the benchmark prediction
  breaks <- (1:nt2)/nt2 - 1 / (2 * nt2)
  interpolation <- sapply(sort(unique(ground_truth$day)), 
                      simplify = T,
                      FUN = function(day_){
                        d <- subset(ground_truth, day == day_, drop = F)
                        if(nrow(d) == 1){
                          c(rep(d$x, nt2), rep(d$y, nt2))
                        }else{
                          c(approx(d$f_start, y = d$x, xout = breaks, rule = 2)$y,
                            approx(d$f_start, y = d$y, xout = breaks, rule = 2)$y
                          )
                        }
                      }) %>% t()
  rownames(interpolation) <- sort(unique(ground_truth$day))
  return(interpolation)
}