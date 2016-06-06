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
nummer <- 174

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
unten <- "2015-05-13"
oben <- "2015-05-14"

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
                                      ) > 13)], group = "GPS")  %>%
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
plot(density(log(intervals + 1)), xaxt = 'n', ylab = "Time",
     main = "Density of intervals between CDR events",
     xlab = "Time")
axis(1, at = log(c(61, 3601, 3600*24+1)),
     labels = c("1 Minute", "1 hour", "1 day"))

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
plot(density(phone_usage_by_person$dens), xlab = "CDR per day",
     main = "Number of daily CDR events by user")
rug(phone_usage_by_person$dens)

plot(density(phone_usage_by_person$days), xlab = "Days",
     main = "Total number of days under observation")
rug(phone_usage_by_person$days)

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
plot(frequencies$Group.1, frequencies$prob, type = "b", 
     xlab = "# of days running", ylab = "Probability of pause",
     main = "Pausing probabilities")
lines(smooth.spline(frequencies$Group.1, frequencies$prob), lwd = 2, col = 4)

  
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
