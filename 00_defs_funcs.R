# ------------------------------------------------------------------------------
# --- Definitions, names, lists ------------------------------------------------
# ------------------------------------------------------------------------------

# Spaltennamen der einzelnen Events
names_prefix <- c("uid", "milisec", "eventtype")
col_headers <- list(
  app.heartbeat = names_prefix,
  app.start = names_prefix,
  call.incoming.answered = c(names_prefix,
                             'number',
                             'time_start',
                             'time_answer',
                             'time_end'),
  call.incoming.missed = c(names_prefix,
                           'number',
                           'time_start',
                           'time_answer',
                           'time_end'),
  call.outgoing = c(names_prefix,
                    'number',
                    'time_start',
                    'time_answer',
                    'time_end'),
  device.airplane.off = names_prefix,
  device.airplane.on  = names_prefix,
  device.cell.location = c(names_prefix,
                           'type',
                           'cid',
                           'lac',
                           'operator_numeric'),
  device.cell.service = c(names_prefix,
                          'state',
                          'operator_short',
                          'operator_long',
                          'roaming'),
  device.network = c(names_prefix,
                     'connected',
                     'roaming',
                     'wifi'),
  sensor.gps = c(names_prefix,
                 'time2',
                 'loc_y',
                 'loc_x',
                 'accuracy',
                 'altitude',
                 'bearing',
                 'speed'),
  sms.incoming = c(names_prefix,
                   'number',
                   'time',
                   'text_length',
                   'text_words',
                   'text_average_word_length',
                   'text_letters',
                   'text_numbers',
                   'text_puncts',
                   'text_whites',
                   'is_email',
                   'is_status_report',
                   'is_replace',
                   'is_mwi'),
  sms.outgoing = c(names_prefix,
                   'number',
                   'time',
                   'text_length',
                   'text_words',
                   'text_average_word_length',
                   'text_letters',
                   'text_numbers',
                   'text_puncts',
                   'text_whites'),
  user.prefs.pause = names_prefix  #,
  # device.battery.level = c(names_prefix,
  #                          'level',
  #                          'max_level',
  #                          'plugged',
  #                          'status')
)
  
# ------------------------------------------------------------------------------
# --- Functions ----------------------------------------------------------------
# ------------------------------------------------------------------------------

# --- Connecting to DB ---------------------------------------------------------
disconnect <- function(){
  library(RPostgreSQL)
  # This function disconnects from the database
  dbDisconnect(con)
  dbUnloadDriver(drv)
}

connect <- function(user = "burkhard", pw = "", dbname = "burkhard", 
                    host = "localhost", port = 5432){
  # This function connects to the database with the given credentials
  # It assigns the relevant variables to the global environment
  library(RPostgreSQL)
  try(disconnect(), silent = T)
  drv <<- dbDriver("PostgreSQL")
  con <<- dbConnect(drv, dbname = dbname,
                    host = host, port = port,
                    user = user, password = pw)
  dbGetQuery(con, "SET TIME ZONE 'UTC';")
}

# --- Importing a csv-file -----------------------------------------------------
import_csv <- function(name_, namelist_, filepath_){
  # takes the name of a csv file and a namelist_, which contains the expected
  #   variables. Then it
  # 1. creates a table in the db with the same name and the variables needed
  # 2. loads the csv into the db
  print(paste("Working on", name_))
  table_name <- gsub("\\.", "_", name_)
  col_defs <- paste(namelist_[[name_]], "varchar (50)", 
                        collapse = ", ")
  query <- paste(" DROP TABLE IF EXISTS", table_name, "CASCADE;
CREATE TABLE", table_name, "(
  ", col_defs, ")")
  dbGetQuery(con, query)
  query_copy <- paste(
    "COPY", table_name, "FROM", 
    paste0("'", filepath_, "/", name_,".csv'"), 
            " DELIMITER ',' NULL \'\\N\' CSV HEADER;")
  tryCatch({
    dbGetQuery(con, query_copy)
    if("milisec" %in% namelist_[[name_]]){
      q <- paste0("ALTER TABLE ", table_name, 
               " ADD COLUMN t timestamp with time zone;
               UPDATE ", table_name, " SET t = TO_TIMESTAMP(milisec::double 
                 precision / 1000);
               ALTER TABLE ", table_name, " ALTER COLUMN uid 
                  SET DATA TYPE numeric USING uid::numeric;
               ALTER TABLE ", table_name, " DROP COLUMN milisec;
               CREATE INDEX ", table_name, "_idx_t ON ", table_name, " (t);
               CREATE INDEX ", table_name, "_idx_uid ON ", table_name, " (uid);")
      dbGetQuery(con, q)}},
    error = function(e) {
      print("Upload failed.")
    },
    warning = function(w){
      print("There was a warning.")
    }
  )
  return(NULL)
}

# --- Voronoi Polygons ---------------------------------------------------------
# Taken from Stackoverflow (correct the pznall for poly)
voronoipolygons <- function(x,poly, eps = 10^-9) {
  library(deldir)
  if (.hasSlot(x, 'coords')) {
    crds <- x@coords  
  } else crds <- x
  bb = bbox(poly)
  rw = as.numeric(t(bbox(poly)))
  z <- deldir(crds[,1], crds[,2],rw=rw)
  w <- tile.list(z)
  polys <- vector(mode='list', length=length(w))
  library(sp)
  for (i in seq(along=polys)) {
    pcrds <- cbind(w[[i]]$x, w[[i]]$y)
    pcrds <- rbind(pcrds, pcrds[1,])
    polys[[i]] <- Polygons(list(Polygon(pcrds)), ID=as.character(i))
  }
  # SP <- SpatialPolygons(polys)
  voronoi <- SpatialPolygons(polys, proj4string = CRS("+init=epsg:3301"))
  
  # voronoi <- SpatialPolygonsDataFrame(SP, 
  #    data=data.frame(x=crds[,1],
  #                    y=crds[,2], 
  #                    row.names=sapply(slot(SP, 'polygons'), 
  #                                     function(x) slot(x, 'ID'))))
  return(voronoi)
}

# --- Write spatial table to Postgis -------------------------------------------
# https://philipphunziker.wordpress.com/2014/07/20/transferring-vector-data-between-postgis-and-r/
# added srid and spatial index in database.

dbWriteSpatial <- function(con, spatial.df, schemaname="public", tablename, 
                           replace=FALSE, srid = 4326) {
  library(rgeos)
  library(RPostgreSQL)
  # con:          A PostgreSQL connection (from RPostgreSQL)
  # spatial.df:   A Spatial Data Frame object
  # schemaname:   Target schema name
  # tablename:    Target table name
  # replace:      Replace the target table if it already exists
  
  # Create well known text and add to spatial DF
  spatialwkt <- writeWKT(spatial.df, byid=TRUE)
  spatial.df$wkt <- spatialwkt
  
  # Add temporary unique ID to spatial DF
  spatial.df$spatial_id <- 1:nrow(spatial.df)
  
  # Set column names to lower case
  names(spatial.df) <- tolower(names(spatial.df))
  
  # Upload DF to DB
  data.df <- spatial.df@data
  rv <- dbWriteTable(con, c(schemaname, tablename), data.df, overwrite=replace, 
                     row.names=FALSE)
  
  # Create geometry column and clean up table
  schema.table <- paste(schemaname, ".", tablename, sep="")
  query1 <- paste("ALTER TABLE ", schema.table, 
                  " ADD COLUMN geom GEOMETRY;", sep="")
  query2 <- paste("UPDATE ", schema.table, 
                  " SET geom = ST_GeomFromText(t.wkt, ", srid, ") FROM ", 
                  schema.table, " t  WHERE t.spatial_id = ", schema.table, 
                  ".spatial_id;", sep="")
  query3 <- paste("ALTER TABLE ", schema.table, " DROP COLUMN spatial_id;")
  query4 <- paste("ALTER TABLE ", schema.table, " DROP COLUMN wkt;")
  query5 <- paste0("CREATE INDEX ", tablename, "sp_index ON ", schema.table, 
                  " USING GIST (geom);")
  er <- dbSendQuery(con, statement=query1)
  er <- dbSendQuery(con, statement=query2)
  er <- dbSendQuery(con, statement=query3)
  er <- dbSendQuery(con, statement=query4)
  er <- dbSendQuery(con, statement=query5)
  
  return(TRUE)
}

# --- Functions for the presentation -------------------------------------------

naive_segmentation <- function(df, dt, dsp, dt_break, dt_short, ds_short){
  # df        data frame with (ordered) points. contains t, x and y
  # dt        time threshold for first segmentation
  # dsp       spatial threshold for first segmentation
  # dt_break  time threshold to split a move segment if distance between two
  #            points is too large
  # dt_short  time threshold for too short moves
  # ds_short  spatial threshold for too short moves
  
  library(plyr)
  library(geosphere)
  df <- df[order(df$t), ]
  n <- nrow(df)
  diff_sp <- diff_t <- array(NA, dim = c(n, 2 * dt + 1))
  diff_sp[, dt + 1] <- T
  diff_t[, dt + 1] <- 0
  # Links der Mitte (nach hinten schauen)
  for(i_ in 1:dt){
    diff_sp[, dt + 1 - i_] <- c(rep(F, i_), 
                                distHaversine(df[-c(1:i_), c("x", "y")],
                                              df[-c((n - i_ + 1):n), 
                                                 c("x", "y")]) < dsp)
    diff_t[, dt + 1 - i_] <- c(rep(0, i_),
                               difftime(df[-c((n - i_ + 1):n), c("t")],
                                        df[-c(1:i_), c("t")],
                                        units = "secs"))
  }
  # Rechts der Mitte (nach vorne schauen)
  for(i_ in 1:dt){
    diff_sp[, dt + 1 + i_] <- c(distHaversine(df[-c((n - i_ + 1):n), 
                                                 c("x", "y")],
                                              df[-c(1:i_), c("x", "y")]) < 
                                  dsp,rep(F, i_))
    diff_t[, dt + 1 + i_] <- c(difftime(df[-c(1:i_), c("t")],
                                        df[-c((n - i_ + 1):n), c("t")],
                                        units = "secs"), 
                               rep(0, i_))
  }
  # Test, ob die Differenzen schön aufsteigend sind (ausser an den Ecken)
  # sum(apply(diff_t, 1, function(v) any(diff(v) < 0 ))) == 2 * (dt-1)
  
  diff_sp[, (dt + 1):(2*dt + 1)] <- t(apply(diff_sp[, (dt + 1):(2*dt + 1)], 
                                            1, 
                                            FUN = function(v) {
                                              as.logical(cumprod(v))
                                            }))
  diff_sp[, 1:dt] <- t(apply(diff_sp[, 1:dt],
                             1, 
                             FUN = function(v) rev(cumprod(rev(v)))) == 1)
  #   Test, ob auf einem Sample nicht zu oft hin und her gewechselt wird.
  #   all(diff_sp[, dt + 1])
  #   max(apply(diff_sp[, (dt + 1):(2 * dt + 1)], 1, 
  #             function(v) sum(abs(diff(v))))) == 1
  #   max(apply(diff_sp[, 1:(dt + 1)], 1, 
  #             function(v) sum(abs(diff(v))))) == 1
  diff_t[!diff_sp]  <- 0
  
  df$stop <- apply(diff_t, 1, FUN = function(v) max(v) - min(v) >= (dt * 60))  
  
  
  # remove short movement segments if they are surrounded by stops that
  #   are close together in space and time
  df$segment = c(0, cumsum(abs(diff(df$stop))))
  temp <- ddply(df, .variables = c("segment", "stop"), .fun = function(df_){
    df_ <- df_[order(df_$t), ]
    n <- nrow(df_)
    return(data.frame(segment = df_[1, "segment"],
                      stop = df_[1, "stop"],
                      t_beg = df_[1, "t"],
                      x_beg = df_[1, "x"],
                      y_beg = df_[1, "y"],
                      t_end = df_[n, "t"],
                      x_end = df_[n, "x"],
                      y_end = df_[n, "y"],
                      gps_id = df_[1, "gps_id"]))
  })
  
  sandwich <- !temp$stop & c(F, temp$stop[-nrow(temp)]) & c(temp$stop[-1], F) &
    difftime(c(temp$t_beg[-1], temp$t_beg[nrow(temp)]),
             c(temp$t_end[1], temp$t_end[-nrow(temp)]), 
             units = "secs") < dt_short & 
    distHaversine(temp[c(2:nrow(temp), nrow(temp)), c("x_beg", "y_beg")], 
                  temp[c(1, 1:(nrow(temp) - 1)), c("x_end", "y_end")]) < ds_short
  df[df$segment %in% temp[sandwich, "segment"], "stop"] <- T
  
  # Zuteilung der Segmente
  df$segment = c(0, cumsum(abs(diff(df$stop))))
  
  # Split segments with long breaks in them
  for(i_ in 1:(nrow(df)-1)){
    if(as.numeric(abs(difftime(df$t[i_], 
                               df$t[i_ + 1], units = "mins"))) > dt_break & 
       df$segment[i_] == df$segment[i_ + 1]){
      df$segment[(i_ + 1):nrow(df)] <- df$segment[(i_+1):nrow(df)] + 1
    }
  }
  
  # df$segment = c(0, cumsum(abs(diff(df$segment))))
  
  return(df)
}

# --- Simplification -----------------------------------------------------------
# Gets simple summary statistics for each segment
# Sticks together adjacent stop segments that are "too close"
naive_simplification <- function(df, ds, simplify_stops = 0, n = 1){
  df <- df[order(df$t), ]
  segments <- ddply(df, "segment", function(df_){
    return(data.frame(gps_id = df_[1, "gps_id"],
                      gps_id_end = df_[nrow(df_), "gps_id"],
                      tmin = df_[1, "t"],
                      tmax = df_[nrow(df_), "t"],
                      stop = df_[1, "stop"],
                      x_mean = mean(df_[, "x"]),
                      y_mean = mean(df_[, "y"]),
                      x_start = df_[1, "x"],
                      x_end = df_[nrow(df_), "x"],
                      y_start = df_[1, "y"],
                      y_end = df_[nrow(df_), "y"]))
  })
  
  # --- Clean up segments
  seg2 <- segments
  j_ <- 1
  for(i_ in 2:nrow(segments)){
    # If a stop follows a stop and they are close, put them together
    #   Exponential weighting to make matches more probable
    if(seg2[j_, "stop"] == T & segments[i_, "stop"] == T & 
       distHaversine(seg2[j_, c("x_mean", "y_mean")], 
                     segments[i_, c("x_mean", "y_mean")]) < ds){
      n_ <- segments[i_, ]
      seg2[j_, "tmax"] <- n_$tmax
      seg2[j_, "x_mean"] <- mean(n_$x_mean, seg2[j_, "x_mean"])
      seg2[j_, "y_mean"] <- mean(n_$y_mean, seg2[j_, "y_mean"])
      seg2[j_, "x_end"] <- n_$x_end
      seg2[j_, "y_end"] <- n_$y_end
    }else{
      # A stop followed by a move lasts until the beginning of the move
      if(seg2[j_, "stop"] == T & segments[i_, "stop"] == F & 
         distHaversine(seg2[j_, c("x_mean", "y_mean")], 
                       segments[i_, c("x_start", "y_start")]) < ds){
        seg2[j_, "tmax"] <- segments[i_, "tmin"]
      }
      # Also, if there is nothing to put together, the currently active
      #   segment is changed
      j_ <- j_ + 1
      seg2[j_, ] <- segments[i_, ]
    }
  }
  seg2 <- seg2[1:j_, ]
  
  # --- Identify close stops
  if(simplify_stops > 0){
    # print("Clustering Stop points")
    library(dbscan)
    library(plyr)
    stops <- spTransform(SpatialPoints(subset(seg2, 
                                              stop)[, c("x_mean", "y_mean")],
                           proj4string = CRS("+init=epsg:4326")), 
                         CRS("+init=epsg:3301"))
    clusters <- dbscan(stops@coords, eps = simplify_stops, minPts = n)
    cluster_points <- aggregate(data.frame(stops@coords), 
                                by = list(clusters$cluster),
                                FUN = function(x) apply(as.matrix(x), 
                                                        MARGIN = 2, 
                                                        FUN = mean))
    cluster_points[, c("x_mean", "y_mean")] <- 
      (SpatialPoints(cluster_points[, c("x_mean", "y_mean")],
                    CRS("+init=epsg:3301")) %>% 
      spTransform(CRS("+init=epsg:4326")))@coords
    
    seg2[which(seg2$stop)[clusters$cluster > 0], 
         c("x_mean", "y_mean")] <- 
      as.matrix(cluster_points[(clusters$cluster + 1)[clusters$cluster > 0], 
                               c("x_mean", "y_mean")])
  }
  return(seg2)
}


# 
# 
# 243
# 
# m <- leaflet() %>% addTiles() %>% 
#   addCircleMarkers(data = spTransform(stops[243:243, ], 
#                                       CRS("+init=epsg:4326")))
# m3 <- addCircleMarkers(m, data = SpatialPoints(seg2[seg2$stop, 
#                                                     c("x_mean", "y_mean")], 
#                                                CRS("+init=epsg:4326")), 
#                        col = "red")
# temp <- subset(daten_segmentiert, t >= "2015-09-16" & t < "2015-09-17")
# cols <- rainbow(9)
# allpoints <- SpatialPoints(temp[temp$segment %in% c(483,484), c("x", "y")], CRS("+init=epsg:4326"))
# m4 <- addCircleMarkers(m3, data = allpoints, 
#                        color = c(rep("green", 4), rep("blue", 4), rep("orange", 3)))
# 
# 
# dists <- seg2[seg2$stop, c("x_mean", "y_mean")] - rep(c(26.70986, 58.3809), each = sum(seg2$stop))
# 
# m2
# 
# m <- leaflet() %>% addTiles() %>%
#   addCircleMarkers(data = spTransform(stops, CRS("+init=epsg:4326")))
# m2 <- addCircleMarkers(m, data = spTransform(stops,
#                                           CRS("+init=epsg:4326")),
#                        color = "red")
# # 
# # 
# # for(i_ in 1:(nrow(df)-1)){
# #   if(as.numeric(abs(difftime(df$t[i_], 
# #                              df$t[i_ + 1], units = "mins"))) > dt_break & 
# #      df$segment[i_] == df$segment[i_ + 1]){
# #     df$segment[(i_ + 1):nrow(df)] <- df$segment[(i_+1):nrow(df)] + 1
# #   }
# # }
# # df <- int
# # 
# int <- subset(seg2, gps_id %in% 5305979:5305990)
# int <- subset(daten_segmentiert, gps_id %in% 5305979:5305990)
# int <- subset(df, gps_id %in% 5305979:5305990)
# int
# 
# myp <- SpatialPoints(df[df$gps_id %in% 5305985:5305989, c("x", "y")],
#                      CRS("+init=epsg:4326"))
# m5 <- addCircleMarkers(m, data = myp, color = c("red", rep("orange", 3),
#                                                 "green"), opacity = 1)
# 

# ------------------------------------------------------------------------------
# --- Re-usable functions ------------------------------------------------------
# ------------------------------------------------------------------------------

make_spatial_lines <- function(mat, crs = CRS("+init=epsg:4326")){
  # assumes that mat has two columns with the x and y coordinates
  SpatialLinesDataFrame(                      # Aim: Spatial Dataframe
    SpatialLines(                             # SpatialLines for the geometry
      list(                                   # SpatialLines is a list of Lines
        #   plus a CRS
        Lines(                                # A Multiline (LineS) is a list of 
          #   lines (Line)
          list(
            Line(mat)),             # Actual Line
          ID = "1")), 
      proj4string = crs),  # Only the SpatialLines has a CRS
    data = mat                      # nrow(data)==length(SpatialLines)
  )
}

make_spatial_segments <- function(mat, crs = CRS("+init=epsg:4326")){
  # assumes that m has 4 columns. x1, y1, x2, y2.
  mat <- as.matrix(mat)
  rownames(mat) <- 1:nrow(mat)
  SpatialLinesDataFrame(                     
    SpatialLines(                            
      unname(sapply(rownames(mat), FUN = function(n){
        Lines(list(Line(matrix(mat[n, ], ncol = 2, byrow = T))), ID = n)
      }))
      , 
      proj4string = crs), 
    data = data.frame(mat)                      
  )
}

sigmoid <- function(t) 1/(1+exp(-t))

Mode <- function(x) {
  ux <- unique(x[!is.na(x)])
  ux[which.max(tabulate(match(x, ux)))]
}