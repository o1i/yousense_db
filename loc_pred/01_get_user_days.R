get_user_days <- function(user_, nt_, level_ = "CDR", fill_avg_dist = 300,
                          hour_shift = 3){
  # This function fetches the data to use by user.
  # user_  is the number of the user
  # nt_    is the number of time intervals that a day should be split into
  # level_ is either CDR or GSM and gets the corresponding data (same format)
  
  # Returns a List with the following elements:
  # seen_masts a dataframe with the coordinates of all seen masts
  # days       a dataframe with the names of the masts seen at each time 
  #            interval (one each)
  
  if(level_ == "CDR"){
    # Query to CDR table
    q <- paste0("
              SELECT a.uid, a.id_masts, 
              extract(year  from a.t_start at time zone 'Europe/Tallinn' - interval '", hour_shift, " hours') as y_corr,
              extract(month from a.t_start at time zone 'Europe/Tallinn' - interval '", hour_shift, " hours') as m_corr,
              extract(day   from a.t_start at time zone 'Europe/Tallinn' - interval '", hour_shift, " hours') as d_corr,
              extract(epoch from AGE(a.t_start at time zone 'Europe/Tallinn' - interval '", hour_shift, " hours', 
              (a.t_start at time zone 'Europe/Tallinn' - interval '", hour_shift, " hours')::date)) / 3600/24 as frac,
              st_x(st_transform(b.geom, 3301)) as mast_x, 
              st_y(st_transform(b.geom, 3301)) as mast_y,
              b.neighbour_avg_dist
              FROM all_cdr a LEFT JOIN masts b
              ON a.id_masts = b.id_masts
              WHERE a.uid = ", user_, " AND
              a.id_masts is not NULL 
              ;")
  }else{
    # Query to GSM table
    q <- paste0("
               SELECT a.uid, a.id_masts, 
               extract(year  from a.t_start at time zone 'Europe/Tallinn' - interval '", hour_shift, " hours') as y_corr,
               extract(month from a.t_start at time zone 'Europe/Tallinn' - interval '", hour_shift, " hours') as m_corr,
               extract(day   from a.t_start at time zone 'Europe/Tallinn' - interval '", hour_shift, " hours') as d_corr,
               extract(epoch from AGE(a.t_start at time zone 'Europe/Tallinn' - interval '", hour_shift, " hours', 
               (a.t_start at time zone 'Europe/Tallinn' - interval '", hour_shift, " hours')::date)) / 3600/24 as frac,
               st_x(st_transform(a.geom_mast, 3301)) as mast_x, 
               st_y(st_transform(a.geom_mast, 3301)) as mast_y,
               a.neighbour_avg_dist
               FROM gps_gsm as a
               WHERE a.uid = ", user_, " AND
               a.geom_mast is not NULL
               ;")
  }

t <- dbGetQuery(con,q)
t$neighbour_avg_dist[is.na(t$neighbour_avg_dist)] <- fill_avg_dist

# Get the Masts seen in the dataset
seen_masts <- unique(t[, c("id_masts", "mast_x", "mast_y", 
                           "neighbour_avg_dist")])
rownames(seen_masts) <- seen_masts$id_masts

t$rest <- (t$frac %% (1/nt_))*nt_
t$frac <- t$frac %/% (1/nt_)
t$id_masts <- as.character(t$id_masts)

# The mast closest to the middle of the segment is taken
t2 <- ddply(t, c("uid", "y_corr", "m_corr", "d_corr", "frac"), 
           .fun = function(df){
             df[which.min(abs(df$rest - 0.5)),]
           })
t2$temp <- as.numeric(t2$id_masts)
t2 <- rbind(data.frame(uid = 0, id_masts = 0, y_corr = 0, m_corr = 0, 
                       d_corr = 0, frac = 1:nt_ - 1, mast_x = 0, mast_y = 0,
                       neighbour_avg_dist = 0, rest = 0, temp = 0), t2)
days <- acast(t2[, c("uid", "y_corr", "m_corr", "d_corr", 
                                       "frac", "id_masts")], 
              uid + y_corr + m_corr + d_corr ~ frac, 
      fill = "", 
      value.var = "id_masts",
      fun.aggregate = paste0)[-1, ]
days[days == ""] <- NA
colnames(days) <- (as.numeric(colnames(days)) + hour_shift) %% 24
rownames(days) <- gsub("_", "-",gsub("^[^_]*_", "", rownames(days))) %>%
  as.Date() %>% strftime(format = "%j") %>% as.numeric()
return(list(seen_masts = seen_masts, days = days))
}

# ------------------------------------------------------------------------------
# --- Version with clustering --------------------------------------------------
# ------------------------------------------------------------------------------

get_user_days_c <- function(user_, nt_, level_ = "CDR", fill_avg_dist = 300,
                          hour_shift = 3, clust_eps = 0.175, 
                          clust_minpt_frac = 0.015){
  # This function fetches the data to use by user.
  # user_  is the number of the user
  # nt_    is the number of time intervals that a day should be split into
  # level_ is either CDR or GSM and gets the corresponding data (same format)
  
  # Returns a List with the following elements:
  # seen_masts a dataframe with the coordinates of all seen masts
  # days       a dataframe with the names of the masts seen at each time 
  #            interval (one each)
  
  if(level_ == "CDR"){
    # Query to CDR table
    q <- paste0("
                SELECT a.uid, a.id_masts, 
                extract(year  from a.t_start at time zone 'Europe/Tallinn' - interval '", hour_shift, " hours') as y_corr,
                extract(month from a.t_start at time zone 'Europe/Tallinn' - interval '", hour_shift, " hours') as m_corr,
                extract(day   from a.t_start at time zone 'Europe/Tallinn' - interval '", hour_shift, " hours') as d_corr,
                extract(epoch from AGE(a.t_start at time zone 'Europe/Tallinn' - interval '", hour_shift, " hours', 
                (a.t_start at time zone 'Europe/Tallinn' - interval '", hour_shift, " hours')::date)) / 3600/24 as frac,
                st_x(st_transform(b.geom, 3301)) as mast_x, 
                st_y(st_transform(b.geom, 3301)) as mast_y,
                b.neighbour_avg_dist
                FROM all_cdr a LEFT JOIN masts b
                ON a.id_masts = b.id_masts
                WHERE a.uid = ", user_, " AND
                a.id_masts is not NULL 
                ;")
  }else{
    # Query to GSM table
    q <- paste0("
                SELECT a.uid, a.id_masts, 
                extract(year  from a.t_start at time zone 'Europe/Tallinn' - interval '", hour_shift, " hours') as y_corr,
                extract(month from a.t_start at time zone 'Europe/Tallinn' - interval '", hour_shift, " hours') as m_corr,
                extract(day   from a.t_start at time zone 'Europe/Tallinn' - interval '", hour_shift, " hours') as d_corr,
                extract(epoch from AGE(a.t_start at time zone 'Europe/Tallinn' - interval '", hour_shift, " hours', 
                (a.t_start at time zone 'Europe/Tallinn' - interval '", hour_shift, " hours')::date)) / 3600/24 as frac,
                st_x(st_transform(a.geom_mast, 3301)) as mast_x, 
                st_y(st_transform(a.geom_mast, 3301)) as mast_y,
                a.neighbour_avg_dist
                FROM gps_gsm as a
                WHERE a.uid = ", user_, " AND
                a.geom_mast is not NULL
                ;")
  }
  
  t <- dbGetQuery(con,q)
  t$neighbour_avg_dist[is.na(t$neighbour_avg_dist)] <- fill_avg_dist
  
  # --- Clustering happens here ------------------------------------------------
  # This version first clusteres the masts using dbscan
  t$epsdist <- pmax(0.5, 2.6289316954 - 
                                0.0002054995 * t$neighbour_avg_dist) * 
    t$neighbour_avg_dist
  
  # --- First get info about mast distances
  all_masts <- unique(t[, c("id_masts", "mast_x", "mast_y", "epsdist",
                            "neighbour_avg_dist")])
  rownames(all_masts) <- all_masts$id_masts
  n_ <- nrow(all_masts)
  mast_dists <- matrix(0, ncol = n_, nrow = n_)
  ind <- 1:n_^2
  ind <- ind[((ind - 1) %% n_) + 1 > ((ind - 1) %/% n_ + 1)]
  mast_dists[ind] <- unlist(mclapply(ind, FUN = function(k_) {
    i_ <- ((k_ - 1) %% n_) + 1
    j_ <- ((k_ - 1) %/% n_ + 1)
    sqrt(sum((all_masts[i_, c("mast_x", "mast_y")] -
                all_masts[j_, c("mast_x", "mast_y")])^2)) / (
                  all_masts[i_, "epsdist"] + 
                    all_masts[j_, "epsdist"]
                )
  }, mc.cores = 4))
  mast_dists <- mast_dists + t(mast_dists)
  rownames(mast_dists) <- all_masts$id_masts
  colnames(mast_dists) <- all_masts$id_masts
  
  # --- Then create the distance matrix of the connections
  n_ <- nrow(t)
  conn_dists <- matrix(0, ncol = n_, nrow = n_)
  ind <- 1:n_^2
  ind <- ind[((ind - 1) %% n_) + 1 > ((ind - 1) %/% n_ + 1)]
  conn_dists[ind] <- unlist(mclapply(ind, FUN = function(k_) {
    i_ <- ((k_ - 1) %% n_) + 1
    j_ <- ((k_ - 1) %/% n_ + 1)
    mast_dists[as.character(t[i_, "id_masts"]), as.character(t[j_, "id_masts"])]
  }, mc.cores = 4))
  
  # --- Clustering
  mast_clust <- dbscan(as.dist(conn_dists), eps = clust_eps, 
                       minPts = clust_minpt_frac * nrow(t))
  all_masts$cluster <- mast_clust$cluster[match(rownames(all_masts), t$id_masts)]
  
  locs <- SpatialPoints(all_masts[, c("mast_x", "mast_y")], 
                        CRS("+init=epsg:3301")) %>%
    spTransform( CRS("+init=epsg:4326"))
  inds <- order(all_masts$cluster)
  cols <- c("#666666",brewer.pal(length(unique(all_masts$cluster)) - 1, "Set3"))
  # leaflet() %>% addTiles() %>%
  #   addCircleMarkers(data = locs[inds],
  #                    color = cols[all_masts$cluster + 1][inds],
  #                    opacity = 1,
  #                    popup = rownames(all_masts)[inds]) %>%
  #   add_colored_points(user_, 10, places_info)

  # --- Get cluster polygons
  library(sp)
  library(rgeos)
  cl_poly_list <- sapply(setdiff(unique(mast_clust$cluster), 0), function(cl_){
    pts <- subset(all_masts, cluster == cl_, drop = F)[, c("mast_x", 
                                                           "mast_y",
                                                           "epsdist"), drop = F]
    sapply(1:nrow(pts), function(i_){
      SpatialPoints(pts[i_, c("mast_x", "mast_y"), drop = F],
                    CRS("+init=epsg:3301")) %>%
        gBuffer(width = max(300, pts[i_, "epsdist"] / 4), id = runif(1))
    }) %>% 
      do.call(what = rbind) %>%
      gUnaryUnion(id = rep(cl_, nrow(pts))) 
    
    # hull <- chull(pts)
    # Polygons(list(Polygon(pts[c(hull, hull[1]), ], hole = F)), ID = cl_)
  }) %>%
    do.call(what = rbind)  # %>% spTransform(CRS("+init=epsg:4326"))-> sp3
    # SpatialPolygons(proj4string = CRS("+init=epsg:3301")) %>%
    # gBuffer(byid = T, width = 300) %>%
    # spTransform(CRS("+init=epsg:4326"))
  
  # leaflet() %>% addTiles() %>% addPolygons(data = sp1) %>% addPolygons(data = sp2, col = "red") %>% addPolygons(data = sp3, col = "green", popup = names(sp3))
  
  # Add "masts" as cluster centres and cluster centres to cl_poly_list
  add <- t(sapply(unique(all_masts$cluster[all_masts$cluster > 0]), function(no_){
    c(no_, mean(all_masts[match(no_, all_masts$cluster), "mast_x"]),
      mean(all_masts[match(no_, all_masts$cluster), "mast_y"]), 0, 0, no_)
  }))
  rownames(add) <- unique(all_masts$cluster[all_masts$cluster > 0])
  colnames(add) <- colnames(all_masts)
  all_masts <- rbind(all_masts, add)
  cl_poly_df <- SpatialPolygonsDataFrame(cl_poly_list, data = subset(all_masts, id_masts < 100))
  
  # Replace masts by cluster_id and add cluster_id
  t$id_masts[mast_clust$cluster > 0] <- mast_clust$cluster[mast_clust$cluster > 0] 
  
  # --- End of clustering ------------------------------------------------------
  
  t$rest <- (t$frac %% (1/nt_))*nt_
  t$frac <- t$frac %/% (1/nt_)
  t$id_masts <- as.character(t$id_masts)
  
  # The mast closest to the middle of the segment is taken
  t2 <- ddply(t, c("uid", "y_corr", "m_corr", "d_corr", "frac"), 
              .fun = function(df){
                if(any(df$id_masts < 100)){
                  df <- subset(df, id_masts < 100, drop = F)
                }
                df[which.min(abs(df$rest - 0.5)),, drop = F]
              })
  
  t2$temp <- as.numeric(t2$id_masts)
  t2 <- rbind(data.frame(uid = 0, id_masts = 0, y_corr = 0, m_corr = 0, 
                         d_corr = 0, frac = 1:nt_ - 1, mast_x = 0, mast_y = 0,
                         neighbour_avg_dist = 0, epsdist = 0, rest = 0, 
                         temp = 0), t2)
  days <- acast(t2[, c("uid", "y_corr", "m_corr", "d_corr", 
                       "frac", "id_masts")], 
                uid + y_corr + m_corr + d_corr ~ frac, 
                fill = "", 
                value.var = "id_masts",
                fun.aggregate = paste0)[-1, ]
  days[days == ""] <- NA
  colnames(days) <- (as.numeric(colnames(days)) * (24/nt_) + hour_shift) %% 24
  rownames(days) <- gsub("_", "-",gsub("^[^_]*_", "", rownames(days))) %>%
    as.Date() %>% strftime(format = "%j") %>% as.numeric()
  return(list(seen_masts = all_masts, days = days, polys = cl_poly_df))
  }




