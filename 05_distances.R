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
EXPLAIN (verbose, analyze, buffers) CREATE TABLE gps_gsm as
SELECT a.uid, a.gps_id, b.id_dcl, c.id_masts, a.geom as geom_gps,
  GREATEST(a.t, b.t) as t_start,
	LEAST(a.t_next, b.t_next) as t_end, b.net, c.geom as geom_mast,
  c.neighbour_avg_dist, c.neighbour_max_dist, c.neighbour_area, c.samples,
  c.radio_level,
  ST_DISTANCE(ST_TRANSFORM(a.geom, 3301), ST_TRANSFORM(c.geom, 3301)) as dist
FROM
  gps a
  JOIN device_cell_location b
  ON a.uid = b.uid and (a.day = b.day or a.day = b.day_next) and 
    b.t <= a.t_next and b.t_next > a.t and
    b.t_next is not null and a.t_next is not null
  JOIN masts c
  ON b.id_masts = c.id_masts
WHERE c.mcc = 248
;"
Sys.time()
system.time(t <- dbGetQuery(con,q))
t
# Over night to 2016-06-01: 9259, i.e. 2.6 h
# 2016-06-03 after some optimisation: 7850, i.e. 2.1h
# 2016-06-10 1h when indexing on days

q <- "
CREATE INDEX gps_gsm_uid ON gps_gsm(uid);
CREATE INDEX gps_gsm_time ON gps_gsm(t_start, t_end);
ALTER TABLE gps_gsm ADD CONSTRAINT gps_gsm_ids PRIMARY KEY (id_masts,
  gps_id, t_start, t_end);
"
Sys.time()
system.time(t <- dbGetQuery(con,q))

# ------------------------------------------------------------------------------
# --- Analysis -----------------------------------------------------------------
# ------------------------------------------------------------------------------

Sys.time()
system.time({
  q <- "
  select gps_id, id_masts, round(neighbour_avg_dist, 1) as neighbour_avg_dist, 
	round(neighbour_max_dist, 1) as neighbour_max_dist, 
	round(neighbour_area, 1) as neighbour_area, 
	radio_level, round(dist::numeric, 1) as distance from gps_gsm;
  ;"
  connections <<- dbGetQuery(con,q)
})

plot(density(log10(connections$neighbour_avg_dist), 
             na.rm = T, adjust = 4), col = 4)
lines(density(log10(connections$neighbour_max_dist), 
             na.rm = T, adjust = 4), col = 2)
# --> Bimodal distribution between short range (<1500m) and the rest
# --> insert a middle group between 10^2.75 and 10^3.2 as "middle"
plot(density(connections$neighbour_max_dist/connections$neighbour_avg_dist,
             na.rm = T, adjust = 5))
# --> the differences between max and avg are not that significant and fairly
#     constant. probably one can take either.

# --- Test for state = 0
i_state0 <- connections$state == 0  # about 95% are state 0
f_state0 <- ecdf(connections$distance[i_state0] / 
                   connections$neighbour_avg_dist[i_state0])
f_state1 <- ecdf(connections$distance[!i_state0] / 
                   connections$neighbour_avg_dist[!i_state0])
plot( 1:10, f_state0(1:10), type = "b", col = "darkgreen")
lines(1:10, f_state1(1:10), type = "b", col = "lightgreen")
# --> surprisingly little difference between the states

# --- Classification by avg distances
i_low  <- connections$neighbour_avg_dist <  10^2.75
i_mid  <- connections$neighbour_avg_dist >= 10^2.75 & 
          connections$neighbour_avg_dist < 10^3.2 
i_high <- connections$neighbour_avg_dist >= 10^3.2
i_all <- 1 + (connections$neighbour_avg_dist > 10^2.75) + 
  (connections$neighbour_avg_dist > 10^3.2)
f_avg_dist_1 <- ecdf(connections$distance[i_low] / 
                       connections$neighbour_avg_dist[i_low])
f_avg_dist_2 <- ecdf(connections$distance[i_mid] / 
                       connections$neighbour_avg_dist[i_mid])
f_avg_dist_3 <- ecdf(connections$distance[i_high] / 
                       connections$neighbour_avg_dist[i_high])
plot(1:10, f_avg_dist_1(1:10), type = "b", col = "darkblue")
lines(1:10, f_avg_dist_2(1:10), type = "b", col = 4)
lines(1:10, f_avg_dist_3(1:10), type = "b", col = "lightblue")

f_max_dist_1 <- ecdf(connections$distance[i_low] / 
                       connections$neighbour_max_dist[i_low])
f_max_dist_2 <- ecdf(connections$distance[i_mid] / 
                       connections$neighbour_max_dist[i_mid])
f_max_dist_3 <- ecdf(connections$distance[i_high] / 
                       connections$neighbour_max_dist[i_high])

lines(1:10, f_max_dist_1(1:10), type = "b", col = "darkred")
lines(1:10, f_max_dist_2(1:10), type = "b", col = 2)
lines(1:10, f_max_dist_3(1:10), type = "b", col = "pink")

# question: at given quantiles of gps-connections, does avg or max give smaller
# areas?
q <- seq(0.6, 0.95, by = 0.05)
dists <- t(rbind(quantile(f_avg_dist_1, q),
               quantile(f_avg_dist_2, q),
               quantile(f_avg_dist_3, q),
               quantile(f_max_dist_1, q),
               quantile(f_max_dist_2, q),
               quantile(f_max_dist_3, q)))
rownames(dists) <- q
areas <- sapply(as.character(q), function(qn){
  c(sum((connections$neighbour_avg_dist * 
           dists[qn,     i_all]) ^ 2 * pi / 1e6, na.rm = T),
    sum((connections$neighbour_max_dist * 
           dists[qn, 3 + i_all]) ^ 2 * pi / 1e6, na.rm = T))
})
plot( q, areas[1, ], type = "b", col = 4)
lines(q, areas[2, ], type = "b", col = 2)
# --> max has slightly higher areas overall, so go for average
# --> up until 0.85 it is fairly "cheap", but later it really starts increasing
# --> question: is this due to mislabelled masts?
ind_outliers <- connections$distance > 
  dists["0.9", i_all] * connections$neighbour_avg_dist
length(unique(connections[i_all, "id_masts"])) / 
  length(unique(connections$id_masts))
# --> the top 10% of relative deviations come from 0.01% of the masts. something
#     seems to be wrong here.

connections$rel_dist_avg <- connections$distance / 
  pmax(200, connections$neighbour_avg_dist)
info_by_mast <- as.data.frame(as.matrix(aggregate(
  connections$rel_dist_avg, 
  by = list(connections$id_masts),
  FUN = function(n) c(min(n), mean(n), max(n), length(n)))))
info_by_mast <- subset(info_by_mast,!is.na(x.1))
plot(density(log10(info_by_mast$x.1)))
rug(log10(info_by_mast$x.1))
plot(10^seq(0, 2, l = 20), ecdf(log10(info_by_mast$x.1))(seq(0, 2, l = 20)),
     log = "x")

# --> lets have a look at some of the weirder masts
q <- "
SELECT gps_id, ST_X(geom_gps) AS gps_x, ST_Y(geom_gps) AS gps_y,
ST_X(geom_mast) as mast_x, ST_Y(geom_mast) AS mast_y, id_masts
FROM gps_gsm WHERE id_masts IN (
SELECT id_masts
FROM gps_gsm
GROUP BY id_masts
HAVING min(dist / greatest(100, neighbour_avg_dist)) > 2
)
;"
weird_points <- dbGetQuery(con,q)
numbers <- table(weird_points$id_masts)
example_masts <- dimnames(numbers)[[1]][order(numbers, decreasing = T)[1:8]]
plot_points <- subset(weird_points, id_masts %in% example_masts)
mast_coords <- cbind(aggregate(plot_points$mast_x, 
                               by = list(plot_points$id_masts), 
                               FUN = mean),
                     aggregate(plot_points$mast_y, 
                               by = list(plot_points$id_masts), 
                               FUN = mean))[, -3]

library(sp)
library(leaflet)
library(RColorBrewer)
cols = brewer.pal(8, "Set1")
points <- SpatialPoints(plot_points[, c("gps_x", "gps_y")],
              proj4string = CRS("+init=epsg:4326"))
leaflet() %>% addTiles() %>% 
  addCircleMarkers(data = points, 
                   color = cols[match(plot_points$id_masts, example_masts)],
                   popup = as.character(plot_points$gps_id)) %>%
  addMarkers(mast_coords[, 2], mast_coords[, 3], 
             popup = as.character(mast_coords$Group.1)) %>%
  addCircleMarkers(mast_coords[, 2], mast_coords[, 3],
                   color = "black",
                   fillColor = cols[match(mast_coords$Group.1, example_masts)],
                   fillOpacity = 1)
             














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
select id_dcl, gps_id, id_masts, t_start, state, t_end, ST_X(geom_gps) as gps_x, ST_Y(geom_gps) as gps_y, 
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


