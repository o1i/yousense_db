# This script contains the syntactic preparation for the GPS part of the data
# This comprises the segmentation and the simplification of the raw trajectories

# ------------------------------------------------------------------------------
# --- 1. Front matters
# ------------------------------------------------------------------------------

rm(list = ls())
set.seed(69087)
source("00_defs_funcs.R")
connect(user = "burkhard", 
        pw = scan("nosafe.txt", what = "character"), 
        dbname = "burkhard",
        host = "localhost", 
        port = 5432)

# ------------------------------------------------------------------------------
# --- 2. Set up Segments table
# ------------------------------------------------------------------------------
dbGetQuery(con, "
          DROP TABLE IF EXISTS segments;
          CREATE TABLE segments(
          uid integer,
          segment integer,
          gps_id bigint,
          tmin timestamp with time zone,
          tmax timestamp with time zone,
          stop boolean,
          pos_start geometry(point),
          pos_mean geometry(point),
          pos_end geometry(point)
          )
          ")

# ------------------------------------------------------------------------------
# --- 3. Segmentation + Simplification
# ------------------------------------------------------------------------------
users <- sort(dbGetQuery(con, "SELECT DISTINCT uid FROM gps;")[, 1])
for(user_ in users){
  q <- paste0("
    SELECT gps_id, t, ST_X(geom) as x, 
    ST_Y(geom) as y FROM gps
    WHERE uid = ", user_, " 
    ORDER BY t;")
  daten <- dbGetQuery(con, q)
  
  # --- Segmentation
  daten_segmentiert <- naive_segmentation(daten, 
                                          dt = 5, 
                                          dsp = 60, 
                                          dt_break = 3, 
                                          dt_short = 100, 
                                          ds_short = 100)
  
  # --- Simplification
  segmente <- cbind(uid = user_, naive_simplification(daten_segmentiert, 100))
  
  # --- Upload
  segmente[, "tmin"] <- as.numeric(segmente[, "tmin"])
  segmente[, "tmax"] <- as.numeric(segmente[, "tmax"])
  dbGetQuery(con, "DROP TABLE IF EXISTS temp CASCADE;")
  dbWriteTable(con, "temp", segmente)
  q <- "
  INSERT INTO segments
  SELECT 
    uid::integer, segment::integer, gps_id::bigint, 
    TO_Timestamp(tmin) as tmin, 
    TO_Timestamp(tmax) as tmax, 
    stop::boolean, 
    ST_SetSRID(ST_Point(x_start, y_start), 4326)::geometry(point) as pos_start,
    ST_SetSRID(ST_Point(x_mean, y_mean), 4326)::geometry(point) as pos_mean,
    ST_SetSRID(ST_Point(x_end, y_end), 4326)::geometry(point) as pos_end
  FROM temp;
  "
  dbGetQuery(con, q)
}

