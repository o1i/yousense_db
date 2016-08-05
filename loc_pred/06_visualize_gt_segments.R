vis_gt <- function(user_, eps_coords = 30, minpts_coords = 4, nt_ = 6,
                   hour_shift = 3, save_stuff = F){
  library(dbscan)
  library(colorspace)
  gt_ <- get_segments_gt(user_, hour_shift = hour_shift, include_moves = T)
  
  # --- Snap coords to cluster centers -----------------------------------------
  get_name <- function(df_){apply(cbind(df_[, "x_mean"], df_[, "y_mean"]), 
                                  1, FUN = paste, collapse = ' ')}
  all_coords <- do.call(what = rbind, lapply(gt_, function(df_){
    df_[df_$f_start > 0 & df_$stop, c("x_mean", "y_mean")]
  }))
  all_coords_names <- get_name(all_coords)
  coord_clus <- dbscan(all_coords, eps = eps_coords, minPts = minpts_coords)
  means <- t(sapply(unique(coord_clus$cluster), function(cl_){
    colMeans(all_coords[coord_clus$cluster == cl_, ])
  }))
  rownames(means) <- unique(coord_clus$cluster)
  
  to_replace <- apply(all_coords[coord_clus$cluster > 0, ], 1, 
                      paste, collapse = ' ')
  replacements <- 
    means[as.character(coord_clus$cluster[coord_clus$cluster > 0]), ]
  repl <- function(c_vec){
    i <- match(get_name(c_vec), to_replace)
    c_vec[!is.na(i), ] <- replacements[i[!is.na(i)], ]
    return(c_vec)
  }
  gt_ <- lapply(gt_, function(df_){
    df_$cluster <- coord_clus$cluster[match(get_name(df_), all_coords_names)]
    df_$cluster[!df_$stop] <- -1
    if(any(df_$cluster == 0 & !is.na(df_$cluster))){
      df_$cluster[df_$cluster == 0 & !is.na(df_$cluster)] <- 
        get_name(df_)[df_$cluster == 0 & !is.na(df_$cluster)]
    }
    df_[, c("x_mean", "y_mean")] <- repl(as.matrix(df_[, c("x_mean", "y_mean"), 
                                                       drop = F]))
    return(df_)
  })
  
  # --- Get the colors for the plot --------------------------------------------
  allsegments <- subset(do.call(rbind, gt_), f_start > 0)
  allsegments_name <- get_name(allsegments)
  time_by_pos <- aggregate(as.numeric(difftime(allsegments$t_end, 
                                    allsegments$t_start,
                     units = "days")), 
            by = list(allsegments_name), sum)
  colnames(time_by_pos) <- c("name", "days")
  
  # --- Sleeping places
  sleeping_places <- subset(allsegments, day != day_next & 
                              difftime(t_end, t_start, units = "hours") > 4,
                            drop = F)
  sleeping_places <- time_by_pos[unique(match(get_name(sleeping_places), time_by_pos$name)), ]
  colnames(sleeping_places) <- c("name", "days")
  sleeping_places <- sleeping_places[order(sleeping_places$days, 
                                           decreasing = T), , drop = F]
  if(nrow(sleeping_places) > 0){
    sleeping_places$col <- 
      diverge_hsv(10)[pmin(4, 1:nrow(sleeping_places))]
    sleeping_places$type <- "sleep"
    sleeping_places[1, "type"] <- "sleep_main"
  }else{
    sleeping_places <- data.frame("name" = character(0), 
                                  "days" = numeric(0),
                                  "col" = character(0),
                                  type = character(0))
  }
  
  # --- Working places
  # Defined as places of at least 3h / day on average that are not yet colored
  working_places <- subset(time_by_pos, 
                           time_by_pos$days * 24 > (3 * length(gt_)) & 
                             !name %in% sleeping_places$name, drop = F)
  if(nrow(working_places) > 0){
    working_places$col <- 
      diverge_hcl(10)[pmax(7, 10:(10-nrow(working_places) + 1))]
    working_places <- working_places[order(working_places$days, 
                                           decreasing = T), , drop = F]
    working_places$type <- "work"
    working_places[1, "type"] <- "work_main"
  }else{
    working_places <- data.frame("name" = character(0), 
                                  "days" = numeric(0),
                                  "col" = character(0),
                                 type = character(0))
  }

  # --- Other frequent places
  frequent_places <- time_by_pos[match(get_name(means), time_by_pos$name), ]
  frequent_places <- subset(frequent_places, !is.na(days) & 
                              !name %in% c(working_places$name, 
                                           sleeping_places$name))
  frequent_places <- frequent_places[order(frequent_places$days, 
                                           decreasing = T), , drop = F]
  if(nrow(frequent_places) > 0){
    frequent_places$col <- terrain_hcl(10)[pmin(5, 1:nrow(frequent_places))]
    frequent_places$type <- "frequent"
  }else{
    frequent_places <- data.frame("name" = character(0), 
                                  "days" = numeric(0),
                                  "col" = character(0),
                                  frequent = character(0))
  }
  
  # --- collecting everything and adding stuff for the gam
  colored_places <- rbind(sleeping_places, 
                          working_places, 
                          frequent_places)
  total_time <- sum(sapply(gt_, function(df_){
    sum(df_$f_end - df_$f_start)
  }))
  colored_places$perc <- colored_places$days / total_time
  
  frequencies <- table(do.call(what = c, sapply(gt_, function(df_){
    get_name(df_)
  }))) / length(gt_)
  
  gt_ <- lapply(gt_, function(df_){
    add1 <- colored_places[match(get_name(df_), colored_places$name), 
                          c("col", "type", "perc")] 
    freq <- as.numeric(frequencies)[match(get_name(df_), names(frequencies))]
    df_ <- cbind(df_, add1, freq)
    df_$col[!df_$stop] <- "#666666"
    df_$col[is.na(df_$col)] <- rgb(255, 192, 0, alpha = 125, m = 256)
    df_[!df_$stop, "type"] <- "move"
    df_[is.na(df_$type), "type"] <- "other"
    return(df_)
  })
  
  # --- get CDR information
  q <- paste0("
  SELECT 
  extract(DOY FROM t_start at time zone 'Europe/Tallinn' - INTERVAL '", 
              hour_shift, " hours')::smallint as day, 
  extract(epoch from AGE(t_start at time zone 'Europe/Tallinn' - interval '", 
              hour_shift, " hours', (t_start at time zone 'Europe/Tallinn' - interval '", 
              hour_shift, " hours')::date)) / 3600/24 as f_start,
  CASE when eventtype in ('call.incoming.answered', 'call.incoming.missed',
    'sms.incoming') then 'incoming' else 'outgoing' end as eventtype
  FROM all_cdr
  WHERE uid = ", user_, "
  ;")
  cdr <- dbGetQuery(con,q)
  ind_inc <- cdr$eventtype == "incoming"
  
  doy_first <- c(as.numeric(strftime(as.Date(c(
    "2015-01-01", "2015-02-01", "2015-03-01", 
    "2015-04-01", "2015-05-01", "2015-06-01", 
    "2015-07-01", "2015-08-01", "2015-09-01", 
    "2015-10-01", "2015-11-01", "2015-12-01")), 
    format = "%j")) - 1, 365)
  saturdays <- which(weekdays(strptime(paste(2015, 1:365), 
                                       format = "%Y %j")) == "Saturday")
  
  # --- Visualise --------------------------------------------------------------
  breaks <- matrix(c(1, doy_first[c(4, 7, 10, 4, 7, 10)], 366), ncol = 2)
  for(i in 1:nrow(breaks)){
    if(save_stuff) jpeg(height = 600, width = 900, quality = 100, 
         file = paste0("figures/byuser/usage_vis_u", user_, "_", i, ".jpeg"))
    plot(NULL, ylim = c(1, 0), xlim = c(breaks[i, 1], breaks[i, 2]), 
         ylab = "Time of day", 
         xlab = "Date",
         yaxt = "n", xaxt = "n", 
         main = paste0("Visualisation of user ", user_, " Q", i))
    axis(2, at = seq(0, 1, l = nt_ + 1), 
         labels = round((seq(0, 1, l = nt_ +1) * (24) + hour_shift) %% 24))
    axis(1, at = doy_first, labels = c("J", "F", "M", "A", "M", "J", "J", "A",
                                       "S", "O", "N", "D", "J"))
    rect(saturdays - 1, -1, saturdays + 1, 2, border = NA, lwd = 2, 
         col = "#999999FF")
    
    # --- Add the days
    add_day <- function(doy_){
      df_ <- gt_[[doy_]]
      rect(as.numeric(doy_) - 1, df_$f_start, as.numeric(doy_), df_$f_end,
           density = - 1, col = df_$col, border = NA)
    }
    sapply(names(gt_), add_day)
    box()
    
    points(cdr$day - 0.5, cdr$f_start, lwd = 3, col = (2:3)[ind_inc + 1],
           pch = (2:3)[ind_inc + 1])
    if(save_stuff) dev.off()
  }
  if(save_stuff) jpeg(height = 500, width = 2000, quality = 100, 
                      file = paste0("figures/byuser/usage_vis_u", user_, 
                                    "_all.jpeg"))
  plot(NULL, ylim = c(1, 0), xlim = c(breaks[1, 1], breaks[nrow(breaks), 2]), 
       ylab = "Time of day", 
       xlab = "Date",
       yaxt = "n", xaxt = "n", 
       main = paste0("Visualisation of user ", user_))
  axis(2, at = seq(0, 1, l = nt_ + 1), 
       labels = round((seq(0, 1, l = nt_ +1) * (24) + hour_shift) %% 24))
  axis(1, at = doy_first, labels = c("J", "F", "M", "A", "M", "J", "J", "A",
                                     "S", "O", "N", "D", "J"))
  rect(saturdays - 1, -1, saturdays + 1, 2, border = NA, lwd = 2, 
       col = "#999999FF")
  
  # --- Add the days
  add_day <- function(doy_){
    df_ <- gt_[[doy_]]
    rect(as.numeric(doy_) - 1, df_$f_start, as.numeric(doy_), df_$f_end,
         density = - 1, col = df_$col, border = NA)
  }
  sapply(names(gt_), add_day)
  box()
  
  points(cdr$day - 0.5, cdr$f_start, lwd = 2, col = (2:3)[ind_inc + 1],
         pch = (2:3)[ind_inc + 1])
  if(save_stuff) dev.off()
  
  
  # --- Get the regression stuff -----------------------------------------------
  get_reg <- function(df_, dow, cdr_){
    # assumes a data frame with a single row. that contains all the goodness
    nt2 <- 24*12
    short <- 1/24/4
    long <- 1/24
    grid <- seq(0, 1, l = nt2 + 1)  # one too many for boundaries
    segs <- grid[grid[-1]>df_$f_start & grid[-length(grid)] < df_$f_end]
    
    if(length(segs) == 0){
      return(data.frame(
        tod = numeric(0), end = logical(0), long = logical(0), 
        stop = logical(0), type = character(0),
        perc_tot = numeric(0), freq = numeric(0), dow = character(0), 
        time_since_last_cdr = numeric(0), in_first_segment = logical(0), 
        in_last_segment = logical(0),
        any_cdr = logical(0)
      ))
    }
    
    cdr_ <- rbind( cdr_, c(0, 0, ""))
    cdr_ <- cdr_[order(cdr_$f_start), ]
    dur_last_cdr <- segs - as.numeric(sapply(segs, function(t_){
      if(max(cdr_$f_start) < t_){
        max(as.numeric(cdr_$f_start))
      }else{
        cdr_[which.max(cdr_$f_start > t_) - 1, "f_start"]
      }
    }))
    bool_cdr_in_seg <- apply(outer(segs + 1/nt2, cdr_$f_start[-1], ">") & 
                             outer(segs, cdr_$f_start[-1], "<="), 1, any)
    
    df2 <- data.frame(
      tod = segs, 
      beginning = seq_along(segs) <= (short * nt2),
      end = rev(seq_along(segs)) <= (short * nt2),
      beginning_2 = seq_along(segs) <= (long * nt2) & 
        (length(segs)) >= (long * nt2),
      end_2 = rev(seq_along(segs)) <= (long * nt2) & 
        (length(segs)) >= (long * nt2),
      long = length(segs) >= (long * nt2),
      stop = df_$stop,
      type = df_$type,
      perc_tot = df_$perc,
      freq = df_$freq,
      dow = dow,
      time_since_last_cdr = dur_last_cdr,
      in_first_segment = df_$f_start == 0,
      in_last_segment = df_$f_end == 1,
      any_cdr = bool_cdr_in_seg
    )
    return(df2)
  }
  
  rel_days <- intersect(cdr$day, names(gt_))
  reg_frame <- lapply(as.list(rel_days), function(d_){
    dow <- doy_to_dow(2015, d_)
    cdr_ <- subset(cdr, day == d_)
    lapply(as.list(1:nrow(gt_[[d_]])), function(row_){
      tt <- get_reg(df_ = gt_[[d_]][row_, , drop = F], dow = dow, cdr_ = cdr_)
    }) %>% do.call(what = rbind)
  }) %>% do.call(what = rbind)
  
  return(list(colored_places = colored_places,
              first = get_name(t(sapply(gt_, function(df_){
                df_[1, c("x_mean", "y_mean"), drop = F]
              }))),
              reg_frame = reg_frame))
}

# library(sp)
# library(RColorBrewer)
# spat <- SpatialPoints(all_coords, CRS("+init=epsg:3301")) %>% spTransform(CRS("+init=epsg:4326"))
# cols <- c("#000000FF", rainbow(length(unique(coord_clus$cluster)) - 1))
# leaflet() %>% addTiles() %>%
#   addCircleMarkers(data = spat, color = substr(cols[coord_clus$cluster + 1], 1, 7),
#                    opacity = 1, popup = as.character(coord_clus$cluster))
