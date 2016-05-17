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
  query5 <- paste("CREATE INDEX sp_index ON", schema.table, 
                  "USING GIST (geom);")
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
  
  # remove short movement segments
  df <- ddply(df, "segment", .fun = function(df){
    n <- nrow(df)
    if(difftime(df[n, "t"], df[1, "t"], units = "secs") < dt_short & 
       distHaversine(df[1, c("x", "y")], df[1, c("x", "y")]) < ds_short & 
       df[1, "stop"] == F){
      df[, "stop"] <- T
    }
    return(df)
  })
  df$segment = c(0, cumsum(abs(diff(df$stop))))
  
  return(df)
}

# --- Simplification -----------------------------------------------------------
# Gets simple summary statistics for each segment
# Sticks together adjacent stop segments that are "too close"
naive_simplification <- function(df, ds, simplify_stops = 0){
  df <- df[order(df$t), ]
  segments <- ddply(df, "segment", function(df_){
    return(data.frame(gps_id = df_[1, "gps_id"],
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
  seg2 <- segments
  j_ <- 1
  for(i_ in 2:nrow(segments)){
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
      if(seg2[j_, "stop"] == T & segments[i_, "stop"] == F & 
         distHaversine(seg2[j_, c("x_mean", "y_mean")], 
                       segments[i_, c("x_start", "y_start")]) < ds){
        seg2[j_, "tmax"] <- segments[i_, "tmin"]
      }
      j_ <- j_ + 1
      seg2[j_, ] <- segments[i_, ]
    }
  }
  seg2 <- seg2[1:j_, ]
  if(simplify_stops > 0){
    library(dbscan)
    library(plyr)
    stops <- spTransform(SpatialPoints(subset(seg2, 
                                              stop)[, c("x_mean", "y_mean")],
                           proj4string = CRS("+init=epsg:4326")), 
                         CRS("+init=epsg:3301"))
    clusters <- dbscan(stops@coords, eps = simplify_stops, minPts = 1)
    cluster_points <- aggregate(data.frame(stops@coords), 
                                by = list(clusters$cluster),
                                FUN = function(x) apply(as.matrix(x), 
                                                        MARGIN = 2, 
                                                        FUN = mean))
    seg2[seg2$stop, c("x_mean", "y_mean")] <- 
      as.matrix(cluster_points[clusters$cluster, c("x_mean", "y_mean")])
  }
  return(seg2)
}

# m <- leaflet() %>% addTiles() %>% 
#   addCircleMarkers(data = spTransform(stops, CRS("+init=epsg:4326")))
# m2 <- addCircleMarkers(m, data = spTransform(stops, 
#                                           CRS("+init=epsg:4326")), 
#                        color = "red")












