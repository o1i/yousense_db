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
# dbGetQuery(con, "
#           DROP TABLE IF EXISTS segments;
#           CREATE TABLE segments(
#           uid integer,
#           segment integer,
#           id_gps bigint,
#           id_gps_end bigint,
#           t_start timestamp with time zone,
#           t_end timestamp with time zone,
#           stop boolean,
#           x_mean numeric,
#           y_mean numeric,
#           x_start numeric,
#           x_end numeric,
#           y_start numeric,
#           y_end numeric,
#           valid_start boolean,
#           valid_end boolean,
#           geom_start geometry(point),
#           geom_mean geometry(point),
#           geom_end geometry(point)
#           )
#           ")

# ------------------------------------------------------------------------------
# --- 3. Segmentation + Simplification
# ------------------------------------------------------------------------------
users <- sort(dbGetQuery(con, "SELECT DISTINCT uid FROM gps;")[, 1])
for(user_ in users){
  print(paste("Processing user", which(users == user_), "of", length(users)))
  q <- paste0("
    SELECT id_gps, t, ST_X(geom) as x, 
    ST_Y(geom) as y,
    flag_problem
    FROM gps
    WHERE uid = ", user_, " 
    ORDER BY t;")
  daten <- dbGetQuery(con, q)
  
  # --- Segmentation
  daten_segmentiert <- naive_segmentation(df = daten, 
                                          dt = 10, 
                                          dsp = 60, 
                                          dt_break = 3, 
                                          dt_short = 600, 
                                          ds_short = 300,
                                          ds_really_short = 200,
                                          problem = "flag_problem",
                                          clean_no = 2,
                                          clean_ratio = 2.5)
  
  # --- Simplification
  segmente <- cbind(uid = user_, naive_simplification(daten_segmentiert,
                                                      ds = 100,
                                                      simplify_stops = 60,
                                                      n = 2))
  
  # --- Visualisation ----------------------------------------------------------
  
  # # --- Displaying all remaining stops
  # temp <- segmente[segmente$stop, ]
  # leaflet() %>% addTiles() %>%
  #   addCircleMarkers(data = SpatialPoints(temp[, c("x_mean", "y_mean")],
  #                                         CRS("+init=epsg:4326")),
  #                    popup = as.character(temp$id_gps))
  # 
  # # Study a case
  # case <- 3963300
  # plusminus <- 60
  # (temp_daten <- daten[abs(daten$id_gps - case) < plusminus, ])
  # (temp_daten_segmentiert <- daten_segmentiert[
  #   abs(daten_segmentiert$id_gps - case) < plusminus, ])
  # (temp_segmente <- segmente[abs(segmente$id_gps - case) < plusminus, ])
  # 
  # # Display the original points
  # lines <- SpatialLines(list(Lines(list(Line(
  #   temp_daten_segmentiert[, c("x", "y")])), ID = 1)), CRS("+init=epsg:4326"))
  # leaflet() %>% addTiles() %>%
  #   addCircleMarkers(data = SpatialPoints(daten_segmentiert[
  #     abs(daten_segmentiert$id_gps - case) < plusminus, c("x", "y")],
  #                                         CRS("+init=epsg:4326")),
  #                    color = c("red", "blue")[1+daten_segmentiert[
  #                      abs(daten_segmentiert$id_gps - case) < plusminus, 
  #                      c("stop")]],
  #                    radius = 5 * (2 - daten_segmentiert[
  #                      abs(daten_segmentiert$id_gps - case) < plusminus, 
  #                      c("stop")]),
  #                    popup = as.character(daten_segmentiert[
  #                      abs(daten_segmentiert$id_gps - case) < plusminus,
  #                                                           c("id_gps")])) %>%
  #   addPolylines(data = lines)
  # 
  # # Display the stops only
  # lines <- SpatialLines(list(Lines(list(Line(segmente[
  #   abs(segmente$id_gps - case) < plusminus, c("x_mean", "y_mean")])), ID = 1)),
  #                       CRS("+init=epsg:4326"))
  # leaflet() %>% addTiles() %>%
  #   addCircleMarkers(data = SpatialPoints(segmente[
  #     abs(segmente$id_gps - case) < plusminus, c("x_mean", "y_mean")],
  #                                         CRS("+init=epsg:4326")),
  #                    color = c("red", "blue")[1+segmente[
  #                      abs(segmente$id_gps - case) < plusminus, c("stop")]],
  #                    radius = 5 * (2 - segmente[
  #                      abs(segmente$id_gps - case) < plusminus, c("stop")]),
  #                    popup = as.character(daten_segmentiert[
  #                      abs(daten_segmentiert$id_gps - case) < plusminus,
  #                                                           c("id_gps")])) %>%
  #   addPolylines(data = lines)
  
  
  # --- Upload -----------------------------------------------------------------
  segmente[, "t_start"] <- as.numeric(segmente[, "t_start"])
  segmente[, "t_end"] <- as.numeric(segmente[, "t_end"])
  dbGetQuery(con, "DROP TABLE IF EXISTS temp CASCADE;")
  dbWriteTable(con, "temp", segmente)
  q <- "
  INSERT INTO segments
  SELECT 
    uid::integer, segment::integer, id_gps::bigint, id_gps_end::bigint,
    TO_Timestamp(t_start) as t_start, 
    TO_Timestamp(t_end) as t_end, 
    stop::boolean,
    x_mean,
    y_mean,
    x_start,
    x_end,
    y_start,
    y_end,
    valid_start::boolean,
    valid_end::boolean,
    ST_SetSRID(ST_Point(x_start, y_start), 4326)::geometry(point) as geom_start,
    ST_SetSRID(ST_Point(x_mean, y_mean), 4326)::geometry(point) as geom_mean,
    ST_SetSRID(ST_Point(x_end, y_end), 4326)::geometry(point) as geom_end
  FROM temp;
  "
  dbGetQuery(con, q)
}

