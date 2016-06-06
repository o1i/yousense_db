# I will need to cluster the masts that belong to the same semantic
#   location in order to sensibly infer my location vectors
# In order to find a sensible clustering, I would like to evaluate the
#   dispersion of mast-locations if the given location is fixed.
# First measure:  Distribution of distance to mast for stop segments
# Second measure: Topological distance between the real cell of a stop and
#                 the connected mast --> difficult --> forget for now
# Third measure:  Csaj-Approach: double the distance between the mast from the
#                 active voronoi cell and see whether the active mast is in
#                 that set.
rm(list=ls())
source("00_defs_funcs.R")
connect(user = "burkhard", 
        pw = scan("nosafe.txt", what = "character"), 
        dbname = "burkhard",
        host = "localhost", 
        port = 5432)

# --- Create the gps_gsm table
q <- "
DROP TABLE IF EXISTS gps_gsm;
EXPLAIN ANALYZE CREATE TABLE gps_gsm as 
SELECT a.uid, a.gps_id, b.id_dcl, c.masts_id, a.state, a.geom as geom_gps, 
  greatest(a.t_start, b.t) as t_start, 
	LEAST(a.t_end, b.t_next) as t_end, b.net, c.geom as geom_mast,
  c.neighbour_avg_dist, c.neighbour_max_dist, c.neighbour_area, c.samples,
  c.radio_level,
  ST_DISTANCE(ST_TRANSFORM(a.geom, 3301), ST_TRANSFORM(c.geom, 3301)) as dist
FROM
  gps_state a 
  JOIN device_cell_location b
  ON a.uid = b.uid and b.t <= a.t_end and b.t_next > a.t_start and 
    b.t_next is not null and a.t_end is not null
  JOIN masts c
  ON b.masts_id = c.masts_id
WHERE c.mcc = 248
;"
Sys.time()
system.time(t <- dbGetQuery(con,q))
t
# Over night to 2016-06-01: 9259, i.e. 2.6 h
# 2016-06-03 after some optimisation: 7850, i.e. 2.1h

q <- "
CREATE INDEX gps_gsm_uid ON gps_gsm(uid);
CREATE INDEX gps_gsm_time ON gps_gsm(t_start, t_end);
"
Sys.time()
system.time(t <- dbGetQuery(con,q))

Sys.time()
system.time({
  q <- "
  select gps_id, masts_id, round(neighbour_avg_dist, 1) as neighbour_avg_dist, 
	round(neighbour_max_dist, 1) as neighbour_max_dist, 
	round(neighbour_area, 1) as neighbour_area, 
	radio_level, round(st_distance::numeric, 1) as distance from gps_gsm_2;
  ;"
  connections <<- dbGetQuery(con,q)
})

connections <- subset(connections, distance < 5000)
f_dist <- ecdf(connections$distance)
plot(1:10000, f_dist(1:10000), log = "x")
f_avg_dist <- ecdf(connections$distance / connections$neighbour_avg_dist)
plot(1:10, f_avg_dist(1:10))
f_max_dist <- ecdf(connections$distance / connections$neighbour_max_dist)
plot(1:10, f_max_dist(1:10))
ind <- sample(nrow(connections), 5000, replace = F)
plot(connections[ind, "neighbour_max_dist"], 
     connections[ind, "distance"], 
     log = "x", cex = 0.2)
abline(0, 1)
abline(0, 2)

# Für die Visualisierung: punkte verbinden
q <- "
ALTER TABLE gps_gsm ADD COLUMN geom_line geometry(linestring);
UPDATE gps_gsm 
SET geom_line = ST_MAKELINE(geom_gps, geom_mast)
;"
system.time(dbGetQuery(con,q))


q <- "
select id_dcl, gps_id, masts_id, t_start, state, t_end, ST_X(geom_gps) as gps_x, ST_Y(geom_gps) as gps_y, 
  ST_X(geom_mast) as mast_x, ST_Y(geom_mast) as mast_y
from gps_gsm
where gps_id < 1000 and t_start <= '2015-04-27 13:00:00' and t_end < '2015-04-27 18:00:00'
;"
system.time(daten <- dbGetQuery(con,q))
points_gps <- SpatialPointsDataFrame(cbind(daten$gps_x, daten$gps_y), 
                                     data = daten[, c("t_start", "t_end")],
                                     proj4string = CRS("+init=epsg:4326"))
lines_gps <- make_spatial_lines(daten[, c("gps_x", "gps_y")])
gsm_conn <- make_spatial_segments(daten[, c("gps_x", "gps_y", "mast_x", "mast_y")])
library(leaflet)
leaflet() %>% addTiles() %>% 
  addPolylines(data = lines_gps) %>%
  addPolylines(data = gsm_conn, color = "purple", opacity = 0.2, weight = 1) %>%
  addCircleMarkers(data = points_gps, 
                   color = c("blue", "red")[1+(daten$state > 0)],
                   radius = c(7, 15       )[1+(daten$state > 0)],
                   popup = as.character(daten$id_dcl))


