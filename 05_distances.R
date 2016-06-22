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

# ------------------------------------------------------------------------------
# --- Analysis -----------------------------------------------------------------
# ------------------------------------------------------------------------------

Sys.time()
system.time({
  q <- "
  select id_gps, id_masts, round(neighbour_avg_dist, 1) as neighbour_avg_dist, 
	round(neighbour_max_dist, 1) as neighbour_max_dist, 
	round(neighbour_area, 1) as neighbour_area, 
	radio_level, round(dist::numeric, 1) as distance, net, samples,
  st_x(geom_gps) as gps_x, st_y(geom_gps) as gps_y,
  st_x(geom_mast) as mast_x, st_y(geom_mast) as mast_y from gps_gsm;
  ;"
  connections <<- dbGetQuery(con,q)
})

# Plot average and max distance of masts (weighted by usage)
jpeg(quality = 100, height = 500, width = 500, 
     file = "figures/mast_dists.jpeg")
plot( density(log10(connections$neighbour_avg_dist[connections$net == 1]), 
             na.rm = T, adjust = 4), lty = 1, col = 1, 
      main = "Density of average/max neighbours dists by provider",
      xlab = "log10 of avg neigh dist")
lines(density(log10(connections$neighbour_max_dist[connections$net == 1]), 
             na.rm = T, adjust = 4), lty = 2, col = 1)
lines(density(log10(connections$neighbour_avg_dist[connections$net == 2]), 
             na.rm = T, adjust = 4), lty = 1, col = 2)
lines(density(log10(connections$neighbour_max_dist[connections$net == 2]), 
              na.rm = T, adjust = 4), lty = 2, col = 2)
lines(density(log10(connections$neighbour_avg_dist[connections$net == 3]), 
             na.rm = T, adjust = 4), lty = 1, col = 3)
lines(density(log10(connections$neighbour_max_dist[connections$net == 3]), 
              na.rm = T, adjust = 4), lty = 2, col = 3)
legend("topright", col = 1:3, legend = c(1:3), lwd = 2)
dev.off()
# --> Bimodal distribution between short range (<1500m) and the rest
# --> insert a middle group between 10^2.75 and 10^3.2 as "middle"
# --> Maybe provider 3 is a bit worse in rural areas, but we will ignore
#     This for now

# Ratio between max and avg
jpeg(quality = 100, height = 500, width = 500, 
     file = "figures/max_div_by_avg.jpeg")
plot(density(connections$neighbour_max_dist/connections$neighbour_avg_dist,
             na.rm = T, adjust = 5), main = "max_neigh_dist / avg_neigh_dist",
     xlab = "quotient")
dev.off()
# --> the differences between max and avg are not that significant and fairly
#     constant. probably one can take either.

# Test for a difference sampling rate between providers.
jpeg(quality = 100, height = 500, width = 500, 
     file = "figures/samples_by_mast_and_provider.jpeg")
plot( density(log10(connections$samples[connections$net == 1]),
              na.rm = T, adjust = 5), col = 1, 
      main = "Density of samples per mast in opencellid by provider",
      xlab = "Number (untransformed!)")
lines(density(log10(connections$samples[connections$net == 2]),
              na.rm = T, adjust = 5), col = 2)
lines(density(log10(connections$samples[connections$net == 3]),
              na.rm = T, adjust = 5), col = 3)
dev.off()
# --> all three telcos have a similar distribution of samples

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
jpeg(quality = 100, height = 500, width = 500, 
     file = "figures/cdf_connections_by_multiples_of_avg_dist.jpeg")
plot(1:10, f_avg_dist_1(1:10), type = "b", col = "darkblue",
     main = "CDF of connections within multiples of avg/max dist.",
     ylab = "Fraction", xlab = "Multiple")
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
legend("bottomright", col = c("darkblue", "blue", "lightblue",
                              "darkred", "red", "pink"),
       legend = c("avg_dense", "avg_medium", "avg_sparse",
                  "max_dense", "max_medium", "max_sparse"), lwd = 2)
dev.off()
# --> In areas where the mast density is low the voronoi cells are already 
#     relatively big, so one does not have to multiply by much.

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
jpeg(quality = 100, height = 500, width = 500, 
     file = "figures/areas_for_quantiles.jpeg")
plot( q, areas[1, ], type = "b", col = 4, 
      main = "Circular area required for a given quantile",
      xlab = "quantile")
lines(q, areas[2, ], type = "b", col = 2)
legend("topleft", col = c(2, 4), lwd = 2, legend = c("max", "avg"))
dev.off()
# --> max has slightly higher areas overall, so go for average
# --> up until 0.85 it is fairly "cheap", but later it really starts increasing
# --> question: is this due to mislabelled masts?
ind_outliers <- connections$distance > 
  dists["0.9", i_all] * connections$neighbour_avg_dist
length(unique(connections[i_all, "id_masts"])) / 
  length(unique(connections$id_masts))
# --> the top 10% of relative deviations come from 0.01% of the masts. something
#     seems to be wrong here.

# What is the distribution of the "true" distance?
jpeg(quality = 100, height = 500, width = 500, 
     file = "figures/dist_from_masts.jpeg")
plot(density(log10(pmax(1, connections$distance)), na.rm = T, adjust = 5),
     main = "Distance from mast",
     xlab = "log10 of distance")
lines(density(log10(pmax(1, connections$distance[i_low])), na.rm = T, adjust = 5), col = 2)
lines(density(log10(pmax(1, connections$distance[i_mid])), na.rm = T, adjust = 5), col = 3)
lines(density(log10(pmax(1, connections$distance[i_high])), na.rm = T, adjust = 5), col = 4)
legend("topright", col = 1:4, 
       legend = c("overall", "dense masts", "medium masts", "sparse masts"), 
       lwd = 2)
dev.off()


# What multiple of avg or max distance should be taken?
connections$rel_dist_avg <- connections$distance / 
  pmax(200, connections$neighbour_avg_dist)
info_by_mast <- as.data.frame(as.matrix(aggregate(
  connections$rel_dist_avg, 
  by = list(connections$id_masts),
  FUN = function(n) c(quantile(n, 0.75, na.rm=T), 
                      mean(n), 
                      quantile(n, 0.9, na.rm=T), length(n)))))
colnames(info_by_mast) <- c("mast", "q75", "mean", "q90", "count")
info_by_mast <- subset(info_by_mast,!is.na(q75))
jpeg(quality = 100, height = 500, width = 500, 
     file = "figures/q_of_dists_vs_avg_neigh_dist.jpeg")
par(mfrow=c(3, 2))

plot(density(log10(info_by_mast$q75)), 
     main = "log10 of q75(distance) / mean distance")
rug(log10(info_by_mast$q75))
plot(10^seq(0, 1, l = 20), ecdf(log10(info_by_mast$q75))(seq(0, 1, l = 20)),
     log = "x", ylab = "ecdf", ylim = c(0, 1), type = "b")

plot(density(log10(info_by_mast$mean)), 
     main = "log10 of mean(distance) / mean distance")
rug(log10(info_by_mast$mean))
plot(10^seq(0, 1, l = 20), ecdf(log10(info_by_mast$mean))(seq(0, 1, l = 20)),
     log = "x", ylab = "ecdf", ylim = c(0, 1), type = "b")

plot(density(log10(info_by_mast$q90)), 
     main = "log10 of q90(distance) / mean distance")
rug(log10(info_by_mast$q90))
plot(10^seq(0, 1, l = 20), ecdf(log10(info_by_mast$q90))(seq(0, 1, l = 20)),
     log = "x", ylab = "ecdf", ylim = c(0 ,1), type = "b")
dev.off()
par(mfrow = c(1, 1))
# --> not too informative

# what are the "isolines" of multipliers for a given quantile of "closer than"?
bin_size <- 200
ti <- connections$neighbour_avg_dist < 10000
x <- aggregate(connections$rel_dist_avg[ti], 
               by = list(connections$neighbour_avg_dist[ti] %/% bin_size),
               FUN = function(v) quantile(v, 
                                          c(0.5, 0.66, 0.75, 0.8, 0.9, 0.95)))

jpeg(quality = 100, height = 500, width = 500, 
     file = "figures/quantile_multipliers_by_avg_neigh_dist.jpeg")
plot( x$Group.1 * bin_size, smooth.spline(x$x[, 6])$y, 
      main = "Multipliers for quantiles", ylim = c(0, 5), type = 'l',
      xlab = "Avg dist to neighbour",
      ylab = "Multiplier to get to a given quantile")
lines(x$Group.1 * bin_size, smooth.spline(x$x[, 5])$y, col = 5)
lines(x$Group.1 * bin_size, smooth.spline(x$x[, 4])$y, col = 4)
lines(x$Group.1 * bin_size, smooth.spline(x$x[, 3])$y, col = 3)
lines(x$Group.1 * bin_size, smooth.spline(x$x[, 2])$y, col = 2)
lines(x$Group.1 * bin_size, smooth.spline(x$x[, 1])$y, col = 1)
points(x$Group.1 * bin_size, x$x[, 4], col = 4, cex = 0.1)
points(x$Group.1 * bin_size, x$x[, 3], col = 3, cex = 0.1)
points(x$Group.1 * bin_size, x$x[, 2], col = 2, cex = 0.1)
points(x$Group.1 * bin_size, x$x[, 1], col = 1, cex = 0.1)
legend("topright", col = 1:6, lwd = 2, legend = paste("q", colnames(x$x)))
dev.off()
# --> effect is surprisingly linear (can also be seen when plotting points)
# --> use this in the distance function
lm(x$x[, "75%"] ~I(x$Group.1 * bin_size))$coefficients
# intercept: 2.6289316954 slope: -0.0002054995 
               
# --> lets have a look at some of the weirder masts
q <- "
SELECT id_gps, ST_X(geom_gps) AS gps_x, ST_Y(geom_gps) AS gps_y,
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
                   popup = as.character(plot_points$id_gps)) %>%
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


