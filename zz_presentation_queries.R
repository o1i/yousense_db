# --- Front matters ------------------------------------------------------------
remove(list = ls())
source("00_defs_funcs.R")
library("RPostgreSQL")
library("leaflet")
try(disconnect(), silent = T)
connect(user = "burkhard", 
        pw = scan("nosafe.txt", what = "character"), 
        dbname = "burkhard",
        host = "localhost", 
        port = 5432)

# --- Actual stuff -------------------------------------------------------------

# --- Parameters
nummer <- 172

# --- GPS Data -----------------------------------------------------------------
q <- paste0("
SELECT t, ST_X(geom) as x, ST_Y(geom) as y, gps_id FROM gps
WHERE uid = ", nummer, ";")
daten <- dbGetQuery(con, q)

# --- Segment the trajectory
daten_segmentiert <- naive_segmentation(daten, 
                                        dt = 5, 
                                        dsp = 100, 
                                        dt_break = 180, 
                                        dt_short = 100, 
                                        ds_short = 100)
segmente <- naive_simplification(daten_segmentiert, 100)




# --- GSM Part -----------------------------------------------------------------
library(rgeos)
q <- paste0("
select a.uid, t, a.cell, a.area, ST_AsText(b.geom_voronoi) as geom,
  ST_AsText(b.geom) as geom_point
from device_cell_location a left join masts b
on 
a.cell = b.cell and 
a.area = b.area and 
a.net = b.net
where a.uid = '", nummer, "'
order by t
;")
daten_gsm <- subset(dbGetQuery(con, q), !is.na(geom))
ids <- 1:nrow(daten_gsm)
rownames(daten_gsm) <- ids
polys_gsm <- do.call(rbind,sapply(ids,
               function(id_){
                 s <- daten_gsm$geom[id_]
                 t <- readWKT(s)
                 slot(t@polygons[[1]], "ID") <- as.character(ids[id_])
                 return(t)
               })) %>% 
  SpatialPolygonsDataFrame(data = daten_gsm[, !colnames(daten_gsm) %in% "geom"],
                           match.ID = F)




# --- CDR Part -----------------------------------------------------------------
get_last_masts <- function(tn_, uid_, timevar_){
  q <- paste0("
select d.*, ST_AsText(e.geom) as point, ST_AsText(e.geom_voronoi) as polygon, '",
              tn_, "' as type from
(select a.uid, a.t, b.cell, b.area, b.mcc, b.net, b.time_connect at time zone 'Europe/Tallinn' as time_connect
from 
  (select uid, ", timevar_, " as t from ", tn_, " where uid = '", uid_, "') a 
left join lateral (
  select uid, cell, area, mcc, net, t as time_connect 
  from device_cell_location temp
  where 
    a.t >= temp.t and
    a.uid = temp.uid
  order by temp.t desc
  limit 1) b
  on a.uid = b.uid) d left join masts e
on 
d.cell = e.cell and 
d.area = e.area and 
d.net= e.net
where d.uid = '", uid_, "' and
e.geom is not null
order by t
;")
get_all_masts  
  
daten_cdr <- dbGetQuery(con, q)
}
sms_in <- get_last_masts("sms_incoming", nummer, "t")
sms_out <- get_last_masts("sms_outgoing", nummer, "t")
tel_in_a <- get_last_masts("call_incoming_answered", nummer, "t")
tel_in_m <- get_last_masts("call_incoming_missed", nummer, "t")
tel_out <- get_last_masts("call_outgoing", nummer, "t")
together <- rbind(sms_in, sms_out, tel_in_a, tel_in_m, tel_out)



# --- Zeitliche Einschränkung --------------------------------------------------
unten <- "2015-05-05"
oben <- "2015-05-06"

# --- GPS
verwendet_gps <- subset(daten_segmentiert, t >= unten & t < oben)
verwendet_segmente_gps <- subset(segmente, tmin < oben & tmax > unten)
traj_gps <- SpatialLines(list(Lines(list(Line(verwendet_gps[, c("x", "y")])), 
                                    ID = "1")),
                         proj4string = CRS("+init=epsg:4326"))
points_gps <- SpatialPointsDataFrame(verwendet_gps[, c("x", "y")],
                                     data = verwendet_gps,
                                     proj4string = CRS("+init=epsg:4326"))

# --- GSM
verwendet_gsm <- polys_gsm[daten_gsm$t >= unten & daten_gsm$t < oben, ]

# --- CDR
verwendet_cdr <- subset(together, t >= unten & t < oben)

getpoints <- function(s){
  c(as.numeric(gsub(" .*", "", gsub(".*\\(", "", s))),
    as.numeric(gsub("\\).*", "", gsub(".* ", "", s))))
}
points <- unname(t(sapply(verwendet_cdr$point, getpoints)))

points_cdr <- SpatialPointsDataFrame(points, data = verwendet_cdr, 
                                     proj4string = CRS("+init=epsg:4326"))

# --- Kartendarstellung --------------------------------------------------------
leaflet() %>% addTiles() %>% addPolylines(data = traj_gps, 
                                          color = c("blue"), group = "GPS") %>%
  addCircleMarkers(data = points_gps, 
                   color = c("blue", "purple", 
                             "red")[1 + 
                                      (as.numeric(strftime(verwendet_gps$t,
                                                           format = "%H")
                                      ) > 11) + 
                                      (as.numeric(strftime(verwendet_gps$t,
                                                           format = "%H")
                                      ) > 13)], group = "GPS",
                   popup = points_gps@data$t)  %>%
  addPolygons(data = verwendet_gsm, group = "GSM") %>%
  addCircleMarkers(data = points_cdr, col = "green", group = "CDR") %>%
  addLayersControl(position = "topleft", 
                   baseGroups = c("GPS", "GSM", "CDR")) -> m
m

# --- Verbindung zwischen GPS und GSM ------------------------------------------



# --- Summary statistics -------------------------------------------------------
# --- Overall phone usage statistics
q <- paste0("select uid, t, 'sms_in' as type from sms_incoming
            union all
            select uid, t, 'sms_out' as type from sms_outgoing
            union all
            select uid, t, 'tel_in_a' as type from call_incoming_answered
            union all
            select uid, t, 'tel_in_m' as type from call_incoming_missed
            union all
            select uid, t, 'tel_out' as type from call_incoming_missed
            order by uid, t"
            )
phone_usage <- dbGetQuery(con, q)
phone_usage$hour <- strftime(phone_usage$t, "%H")
write.table(phone_usage, file = "phone_usage.csv", row.names = F,
            col.names = T, sep = ',')

phone_usage <- 
intervals <- diff(as.numeric(phone_usage$t))[diff(as.numeric(
  phone_usage$uid)) == 0]
jpeg("figures/inter_cdr_time_density.jpeg", quality = 100, height = 300, 
     width = 600)
plot(density(log(intervals + 1)), xaxt = 'n', ylab = "Density",
     main = "Density of intervals between CDR events",
     xlab = "Time (log scale)")
axis(1, at = log(c(61, 3601, 3600*24+1)),
     labels = c("1 Minute", "1 hour", "1 day"))
dev.off()

# --- summary of phone usage per person
q <- paste0("select uid, min(t) as mint, max(t) as maxt, count(*) as anz
from (select uid, t, 'sms_in' as type from sms_incoming
            union all
            select uid, t, 'sms_out' as type from sms_outgoing
            union all
            select uid, t, 'tel_in_a' as type from call_incoming_answered
            union all
            select uid, t, 'tel_in_m' as type from call_incoming_missed
            union all
            select uid, t, 'tel_out' as type from call_incoming_missed) a
            group by a.uid"
)
phone_usage_by_person <- dbGetQuery(con, q)
phone_usage_by_person$days <-  as.numeric(difftime(phone_usage_by_person$maxt, 
                                                   phone_usage_by_person$mint, units = "days"))
phone_usage_by_person$dens <- phone_usage_by_person$anz / 
  as.numeric(difftime(phone_usage_by_person$maxt, phone_usage_by_person$mint, units = "days"))
jpeg("figures/avg_cdr_per_day_by_user.jpeg", 
     quality = 100, height = 300, width = 600)
plot(density(phone_usage_by_person$dens), xlab = "CDR per day",
     main = "Number of daily CDR events by user", lwd = 2)
rug(phone_usage_by_person$dens)
dev.off()

jpeg("figures/days_of_observation_by_user.jpeg",
     quality = 100, height = 300, width = 600)
plot(density(phone_usage_by_person$days), xlab = "Days",
     main = "Total number of days under observation", lwd = 2)
rug(phone_usage_by_person$days)
dev.off()

# --- Pauses: simple
q <- "select uid, count(*) as anz from user_prefs_pause group by uid;"
pauses <- dbGetQuery(con, q)
pauses <- merge(phone_usage_by_person, pauses, by = "uid", all.x = T, all.y = T)
pauses$anz.y[is.na(pauses$anz.y)] <- 0
plot(pauses$days, pauses$anz.y, xlab = "No. of pauses", 
     ylab = "No. of observed days", main = "Pauses summary")

# --- Pauses after t0
q <- "select a.uid, a.t, age(a.t, b.start) as age, b.start
from
  user_prefs_pause a left join
  (select uid, min(t) as start from app_heartbeat group by uid) b
on a.uid = b.uid"
pauses_t0 <- dbGetQuery(con, q)
pauses_t0$age <- round(as.numeric(difftime(pauses_t0$t, pauses_t0$start, units = "days")))
frequencies <- aggregate(pauses_t0$age, by = list(pauses_t0$age), FUN = length)
corr_term <- cbind(0:365, sapply(0:365, function(d){
  sum(phone_usage_by_person$days >= d)
}))
colnames(corr_term) <- c("days", "userdays")
frequencies <- merge(frequencies, corr_term, by.x = "Group.1", 
                     by.y = "days", all.x = T)
frequencies$prob <- frequencies$x / frequencies$userdays

jpeg("figures/prob_of_pause.jpeg", quality = 100, height = 400, width = 600)
par(mar = c(5.1, 4.1, 4.1, 4.1))
plot(frequencies$Group.1, frequencies$prob, type = "p", 
     xlab = "Days after start of recording", ylab = "Probability of pause",
     main = "Pausing probabilities", col = gray(0.6), lwd = 2,
     yaxt = 'n')
ax <- seq(0, 0.4, by = 0.1)
axis(4, at = ax, 
     labels = round(ax / 0.4 * 127),
     col = "#fdae61", lwd = 2)
lines(frequencies$Group.1, frequencies$userdays / 127 * 0.4, 
      lwd = 2, col = "#fdae61")
axis(2, at = ax, col = "#2c7bb6", lwd = 2)
lines(smooth.spline(frequencies$Group.1, frequencies$prob), 
      lwd = 3, col = "#2c7bb6")
mtext("# User days of observation", side=4, line=2)
dev.off()

# --- Mast pictures ------------------------------------------------------------
q <- "select net, radio, neighbour_avg_dist as dist from masts
where neighbour_avg_dist is not null and mcc = 248"
info_masts <- dbGetQuery(con, q)
nets <- unique(info_masts$net)
radios <- unique(info_masts$radio)

cols <- c("#66c2a5", "#fc8d62", "#8da0cb")

jpeg("figures/mast_densities.jpeg", quality = 100, height = 300, width = 600)
plot(NULL, xlim = c(1.5, 4.5), ylim = c(0, 1),
     xlab = "Avg Distance to Voronoi-Neighbour", ylab = "Density")
for(i_ in seq(along = nets)){
  for(j_ in seq(along=radios)){
    lines(density(log10(subset(info_masts, net == nets[i_] & radio == radios[j_])$dist)),
          lty = i_, col = cols[j_], lwd = 2)
  }
}
legend("topleft", lwd = 2, col = cols, legend = radios)
legend("topright", lwd = 2, lty = 1:3, legend = paste("Provider", nets))
dev.off()
  
# --- Toy example for vector clustering
# n <- 200
# p <- 10
# steps <- 1000
# 
# t <- matrix(round(cumsum(rnorm(n*p, sd = 0.5))), 
#             byrow = T, nrow = n)
# loc <- cbind(rnorm(n), rnorm(n)) / 100
# for(i in 1:1000){
#   c <- 0.0001
#   loc <- loc + t(sapply(1:n, function(n_){
#     apply(
#       (c *
#          (loc - rep(loc[n_,], each = n)) *
#          1/apply((loc - rep(loc[n_,], each = n))^2, 1, sum) * 
#          apply(t - rep(t[n_], each = n), 1, function(v) -2*sum(v==0)-p))[-n_,],
#       2, sum)
#   }))
#   if(i%%10==0){
#     plot(loc[, 1], loc[, 2], ylim = c(-15, 15), xlim = c(-15, 15),
#          main = paste("i=", i))
#   }
# }

# ------------------------------------------------------------------------------
# --- Sequential clustering
# ------------------------------------------------------------------------------
# Aim: demonstrate the necessity of multiple epsilon thresholds
set.seed(21587)
radii <- matrix(c(0, 0.3, 0.33, 0.4, 0.45, 0.5, 0.6, 0.7, 0.85, 1), 
                ncol = 2, byrow = T)
N <- 5000
points <- matrix(runif(2 * N, -1, 1), 
                 ncol = 2, dimnames = list(NULL, c("X", "Y")))
rad <- sqrt(points[, 1]^2 + points[, 2]^2)
ind <- rad < 1
points <- points[ind, ]
plot(points)

M <- 750
p1 <- (points[1:M, ]  / 2                + rep(c(-1, 0), each = M))
p2 <- (points[(M + 1):(3 * M), ]         + rep(c(-1, 0), each = 2 * M))
p3 <- (points[(3 * M + 1):(4 * M), ]     + rep(c( 1, 0), each = M))
points_2 <- rbind(p1, p2, p3)
plot(points_2)

cols <- c("#000000", "#377eb8", "#4daf4a", "#e41a1c")
library(dbscan)
jpeg(file = "figures/epsilon_choice.jpeg",
     height = 500, width = 600, quality = 100)
par(mfrow = c(2, 2), oma = c(1, 0, 2.5, 0), mar = c(2, 1, 1, 1))
m1 <- dbscan(points_2, eps =0.1, minPts = 40)
m2 <- dbscan(points_2, eps =0.178, minPts = 40)
m3 <- dbscan(points_2, eps =0.4, minPts = 40)
plot(points_2, col = cols[c(1, 4)][m1$cluster + 1], xlim = c(-2, 3), 
     xaxt = "n", yaxt = "n", asp = 1)
mtext("(a)", side = 1, line = 1)
  text(2, 0.5, expression(epsilon, "  = 0.1"), pos = 4)
plot(points_2, col = cols[c(1, 3)][m2$cluster + 1], xlim = c(-2, 3), 
     xaxt = "n", yaxt = "n", asp = 1)
mtext("(b)", side = 1, line = 1)
  text(2, 0.5, expression(epsilon, "  = 0.18"), pos = 4)
plot(points_2, col = cols[c(1, 2)][m3$cluster + 1], xlim = c(-2, 3), 
     xaxt = "n", yaxt = "n", asp = 1)
mtext("(c)", side = 1, line = 1)
  text(2, 0.5, expression(epsilon, "  = 0.4"), pos = 4)
points_3 <- points_2[m1$cluster == 0, ]
m4 <- dbscan(points_3, eps = 0.178, minPts = 40)
points_4 <- points_3[m4$cluster == 0, ]
m5 <- dbscan(points_4, eps = 0.4, minPts = 40)
mc5 <- max(m5$cluster)
mc4 <- max(m4$cluster)
  plot(points_4, col = cols[m5$cluster + 1], xlim = c(-2, 3), 
       xaxt = "n", yaxt = "n", asp = 1)
  mtext("(d)", side = 1, line = 1)
points(points_3[m4$cluster > 0, ], 
       col = cols[m4$cluster[m4$cluster > 0] + 1 + mc5])
points(points_2[m1$cluster > 0, ], 
       col = cols[m1$cluster[m1$cluster > 0] + 1 + mc5 + mc4])
text(c(2, 2, 2), c(0.5, 0.3, 0.1), 
     c(expression("1)"~epsilon~"= 0.1"), 
       expression("2)"~epsilon~"= 0.18"),
       expression("3)"~epsilon~"= 0.4")),  
     pos = 4)
mtext("Problem of Epsilon choice and a sequential alternative", 
      outer = TRUE, cex = 1.5)
dev.off()
