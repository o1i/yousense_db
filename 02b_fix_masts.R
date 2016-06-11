# This script makes a sensible masts table

source("00_defs_funcs.R")
connect(user = "burkhard", 
        pw = scan("nosafe.txt", what = "character"), 
        dbname = "burkhard",
        host = "localhost", 
        port = 5432)

# ------------------------------------------------------------------------------
# --- Cell tower info ----------------------------------------------------------
# ------------------------------------------------------------------------------

library("RPostgreSQL")

try(disconnect(), silent = T)
connect(user = "burkhard", 
        pw = scan("nosafe.txt", what = "character"), 
        dbname = "burkhard",
        host = "localhost", 
        port = 5432)

q <- "
/* Create the table */
DROP TABLE IF EXISTS masts CASCADE;
CREATE TABLE masts (
  radio text,
  mcc integer,
  net integer,
  area integer,
  cell integer,
  unit integer,
  lon numeric(10, 5),
  lat numeric(10, 5),
  range integer,
  samples integer,
  changeable integer,
  created integer,
  updated integer,
  average_signal integer
);

/* Do the Copying */
COPY masts
FROM '/project-data/userdata/rawdata/cell_towers.csv'
CSV HEADER;

/* Delete unused countries */
DELETE FROM masts
WHERE mcc not in (248, 228);

/* Make the geometries */
ALTER TABLE masts ADD COLUMN geom geometry('POINT');
ALTER TABLE masts ADD COLUMN radio_level numeric;
ALTER TABLE masts ADD COLUMN geom_voronoi geometry('POLYGON');
UPDATE masts
SET geom = ST_SetSRID(ST_Point(CAST(lon as double precision), 
  CAST(lat as double precision)), 4326),
SET radio_level = case when radio = 'LTE' then 3 else 
  case when radio = 'UMTS' then 2 else 1 end end,
SET radio = trim(both from radio);

/* Add Indexes */
ALTER TABLE masts ADD COLUMN id_masts BIGSERIAL PRIMARY KEY;
CREATE INDEX geom_index ON masts USING GIST (geom);
CREATE INDEX id_mastsx_mast_connection ON masts (mcc, net, area, cell);
CREATE INDEX id_masts_partial ON masts(id_masts) WHERE mcc = 248;
"
dbGetQuery(con, q)


# ------------------------------------------------------------------------------
# --- Voronoi Polygons ---------------------------------------------------------
# ------------------------------------------------------------------------------

# library(rgeos)
library(sp)
library(rgdal)
library(magrittr)
q <- "
SELECT id_masts, radio,
  case when mcc = 248 then ST_X(ST_Transform(geom, 3301)) else 
    ST_X(ST_Transform(geom, 2056)) end as x,  
  case when mcc = 248 then ST_Y(ST_Transform(geom, 3301)) else 
    ST_Y(ST_Transform(geom, 2056)) end as y, cell, mcc, net, area from masts
where mcc in (248, 228)
;"
mast_points <- dbGetQuery(con, q)
coordinates(mast_points) <- ~x+y

# --- Get the bounding box for Estonia and transform
SpatialPolygons(list(Polygons(list(Polygon(
  matrix(c(21.771851, 21.771851,  28.20989,  28.20989, 21.771851, 
           57.509312,  59.685749,  59.685749, 57.509312, 57.509312), 
         ncol = 2))), ID = "1")), proj4string=CRS("+init=epsg:4326")) %>% 
  spTransform(CRS("+init=epsg:3301")) -> bbox
bbox_est <- bbox@polygons[[1]]@Polygons[[1]]

# --- BBOX of switzerland
SpatialPolygons(list(Polygons(list(Polygon(
  matrix(c(5.95587, 5.95587,  10.49203,  10.49203, 5.95587, 
           45.81802,  47.80838,  47.80838, 45.81802, 45.81802), 
         ncol = 2))), ID = "1")), proj4string=CRS("+init=epsg:4326")) %>% 
  spTransform(CRS("+init=epsg:2056")) -> bbox
bbox_ch <- bbox@polygons[[1]]@Polygons[[1]]

radios <- unique(mast_points@data$radio)

# --- Do the Voronoi tesselation for every provider
rm(tesselation)
for(mcc_ in c(248, 228)){
  if(mcc_ == 248){
    proj4string(mast_points) <- CRS("+init=epsg:3301")
    the_bbox <- bbox_est
  }else{
    proj4string(mast_points) <- CRS("+init=epsg:2056")
    the_bbox <- bbox_ch
  }
  for(net_ in 1:3){
    for(radio_ in radios){
      set <- mast_points[mast_points@data$mcc == mcc_ & 
                           mast_points@data$net == net_ &
                           mast_points@data$radio == radio_, ]
      set <- set[order(set@data$id_masts), ]
      set@data$temp_id <- 1:length(set)
      set_unique <- set[!duplicated(set@coords), ]
      tesselation_t <- voronoipolygons(set_unique, the_bbox)
      tesselation_t2 <- 
        SpatialPolygonsDataFrame(tesselation_t, 
                                 data = data.frame(net = 
                                                     rep(net_, 
                                                         length(tesselation_t)),
                                                   radio = 
                                                     rep(radio_,
                                                         length(tesselation_t)),
                                                   mcc = 
                                                     rep(mcc_, 
                                                         length(tesselation_t))))
      tesselation_t2 <- spTransform(tesselation_t2, CRS("+init=epsg:4326"))
      
      if(!exists("tesselation")){
        tesselation <- tesselation_t2
      }else{
        n <- length(tesselation)
        tesselation <- rbind(spChFIDs(tesselation, 
                                      as.character(1:n)),
                             spChFIDs(tesselation_t2, 
                                      as.character(
                                        (n + 1):
                                          (n + length(tesselation_t2)))))
      }
    }
  }
}

# --- Show the tesselation on a map
library(leaflet)
# temp <- tesselation
# mp1 <- subset(mast_points, mcc != 248)
# proj4string(mp1) <- CRS("+init=epsg:2056")
# mp <- spTransform(mast_points, CRS("+init=epsg:4326"))
# 
# m <- leaflet() %>% addTiles(group = "OSM") %>%
#   addPolygons(data = temp[temp@data$net == 1,], group = "EMT") %>%
#   addCircles(data = mp[mp@data$net == 1,], group = "EMT") %>%
#   addPolygons(data = temp[temp@data$net == 2,], group = "Elisa", color = "green") %>%
#   addCircles(data = mp[mp@data$net == 2,], group = "Elisa", color = "green") %>%
#   addPolygons(data = temp[temp@data$net == 3,], group = "Tele 2", color = "purple") %>%
#   addCircles(data = mp[mp@data$net == 3,], group = "Tele 2", color = "purple") %>%
#   addLayersControl(baseGroups = c("EMT", "Elisa", "Tele 2"))
# m

# --- Upload the polygons back to the database
dbWriteSpatial(con, tesselation, tablename = "tesselation", replace = T)
q <- "
CREATE INDEX tesselation_idx USING GIST on TESSELATION (geom)
"
dbGetQuery(con, q)
# Test that the cell id is unique (within mcc, net and area)
q <- "SELECT net, area, cell, count(*) as anz
from masts 
group by mcc, net, area, cell, radio
;"
print("Test that cell IDs are unique")
temp <- dbGetQuery(con, q)
max(dbGetQuery(con, q)$anz) == 1
# Test that every (relevant) point is exactly in one polygon
# does not work anymore, since i have 3 tesselations. one for each radio
q <- "SELECT a.net, a.area, a.cell, ST_X(a.geom) as x, ST_Y(a.geom) as y
from masts a left join tesselation b
on ST_Contains(b.geom, a.geom) and a.mcc=b.mcc and a.net = b.net and
a.radio = b.radio
where b.geom is null
group by a.net, a.area, a.cell, a.geom, a.radio
having count(*) = 0
;
"
print("Test that every point is at most one polygon")
nrow(dbGetQuery(con, q)) == 0
# --- Actually adding the voronoi information to the masts
q <- "
UPDATE masts a
SET geom_voronoi = b.geom
from tesselation b
where ST_contains(ST_Transform(b.geom, 3301), 
ST_Transform(a.geom, 3301)) and 
a.mcc = b.mcc and 
a.net = b.net and 
a.radio = b.radio
;
"
t <- dbGetQuery(con, q)

# --- Add summary statistics to the masts --------------------------------------
# Statistics to be added:
# 1. Avg + max distance to neighbours within the same operator and radio
# An alternative would be to allow for "higher" radio neighbours, but the limit
#   Depends on the cellphone, hence implementing it would be impractical
# 2. Avg. Area of the neighboring cells (including the cell itself)

q <- "
ALTER TABLE masts DROP COLUMN IF EXISTS geom_proj;
ALTER TABLE masts DROP COLUMN IF EXISTS geom_voronoi_proj;
ALTER TABLE masts ADD COLUMN geom_proj geometry(point);
ALTER TABLE masts ADD COLUMN geom_voronoi_proj geometry(polygon);
UPDATE masts 
SET geom_proj = ST_TRANSFORM(geom, 3301),
geom_voronoi_proj = ST_TRANSFORM(geom_voronoi, 3301)
WHERE mcc = 248
;
CREATE INDEX masts_geom_proj_idx ON masts USING GIST (geom_proj);
CREATE INDEX masts_geom_voronoi_proj_idx 
ON masts USING GIST (geom_voronoi_proj);
"
dbGetQuery(con, q)

q <- "
ALTER TABLE MASTS DROP COLUMN IF EXISTS neighbour_avg_dist;
ALTER TABLE MASTS DROP COLUMN IF EXISTS neighbour_max_dist;
ALTER TABLE MASTS DROP COLUMN IF EXISTS neighbour_area;

ALTER TABLE MASTS ADD COLUMN neighbour_avg_dist NUMERIC;
ALTER TABLE MASTS ADD COLUMN neighbour_max_dist NUMERIC;
ALTER TABLE MASTS ADD COLUMN neighbour_area NUMERIC;

UPDATE MASTS m1
SET 
  neighbour_avg_dist = m2.avg_dist,
  neighbour_max_dist = m2.max_dist,
  neighbour_area = m2.avg_area
FROM 
  (SELECT a.id_masts, 
    avg(ST_DISTANCE(a.geom_proj, 
    b.geom_proj)) as avg_dist,
    max(ST_DISTANCE(a.geom_proj, 
    b.geom_proj)) as max_dist,
    avg(ST_AREA(b.geom_voronoi_proj)) as avg_area
  FROM masts a left join masts b
  ON ST_TOUCHES(a.geom_voronoi_proj, b.geom_voronoi_proj) and 
    a.radio = b.radio and
    a.net = b.net
  WHERE a.mcc = 248 and b.mcc = 248
  GROUP BY a.id_masts
  ) m2
WHERE 
m1.id_masts = m2.id_masts and 
m1.mcc = 248
;
"
dbGetQuery(con,q)