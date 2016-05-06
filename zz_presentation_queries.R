# --- Front matters ------------------------------------------------------------
remove(list = ls())
source("00_defs_funcs.R")
library("RPostgreSQL")
try(disconnect(), silent = T)
connect(user = "burkhard", 
        pw = scan("nosafe.txt", what = "character"), 
        dbname = "burkhard",
        host = "localhost", 
        port = 5432)

# --- Actual stuff -------------------------------------------------------------

# --- Parameters
nummer <- 203
unten <- "2015-05-05"
oben <- "2015-05-06"

# --- GPS Data -----------------------------------------------------------------
q <- paste0("
SELECT t, ST_X(geom) as x, ST_Y(geom) as y FROM gps
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

# --- Visualise on map 
verwendet <- subset(daten_segmentiert, t >= unten & t < oben)
traj_gps <- SpatialLines(list(Lines(list(Line(verwendet[, c("x", "y")])), 
                                    ID = "1")),
                         proj4string = CRS("+init=epsg:4326"))
points_gps <- SpatialPointsDataFrame(verwendet[, c("x", "y")],
                                     data = verwendet,
                                     proj4string = CRS("+init=epsg:4326"))
leaflet() %>% addTiles() %>% addPolylines(data = traj_gps, 
                                          color = c("blue")) %>%
  addCircleMarkers(data = points_gps, 
                   color = c("blue", "purple", 
                             "red")[1 + 
                                      (as.numeric(strftime(verwendet$t,
                                                           format = "%H")
                                      ) > 11) + 
                                      (as.numeric(strftime(verwendet$t,
                                                           format = "%H")
                                      ) > 13)])  -> m
m

# --- GSM Part -----------------------------------------------------------------
q <- paste0("
select a.uid, t, a.cid, a.lac, ST_AsText(b.geom_voronoi) as geom
from device_cell_location a left join masts b
on 
cast(a.cid as numeric) = b.cell and 
cast(a.lac as numeric) = cast(b.area as numeric) and 
cast(substr(a.operator_numeric, 5, 1) as numeric)= b.net
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

verwendet <- polys_gsm[daten_gsm$t >= unten & daten_gsm$t < oben, ]
leaflet() %>% addTiles() %>% addPolygons(data = verwendet) -> m
m





















