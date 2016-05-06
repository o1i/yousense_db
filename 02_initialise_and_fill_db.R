# This script takes .csv files in the "Sandkasten" directorie and 
# puts them into the database.

library("RPostgreSQL")

try(disconnect(), silent = T)
connect(user = "burkhard", 
        pw = scan("nosafe.txt", what = "character"), 
        dbname = "burkhard",
        host = "localhost", 
        port = 5432)

# --- Copy all the .csv files into the database (all varcar (50))
res <- sapply(names(col_headers), function(name_){
  import_csv(name_, col_headers, sand_path)
})

# --- Creating the "real" tables.
q_gps2 <- "
 DROP TABLE IF EXISTS gps;
  SELECT
    CAST(uid as integer) as uid,
    TO_TIMESTAMP(milisec::double precision / 1000) as t,
    TO_TIMESTAMP(time2::double precision / 1000) as t2,
    CAST(accuracy as double precision) as accuracy,
    CAST(altitude as double precision) as altitude,
    CAST(bearing as double precision) as bearing,
    CAST(speed as double precision) as speed,
    ST_SetSRID(ST_Point(CAST(loc_x as double precision), 
                        CAST(loc_y as double precision)), 4326) as geom
  into gps
  FROM sensor_gps
  ;"
dbGetQuery(con, q_gps2)
q_gps_id <- "
ALTER TABLE gps ADD COLUMN gps_ID BIGSERIAL PRIMARY KEY;
CREATE INDEX gps_sp_index ON gps USING GIST (geom);
CREATE INDEX time ON gps (t)
"
dbGetQuery(con, q_gps_id)

try(disconnect(), silent = T)

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
DROP TABLE IF EXISTS masts CASCADE;
CREATE TABLE masts (
  radio character(10),
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
COPY masts
FROM '/project-data/userdata/rawdata/cell_towers.csv'
CSV HEADER;
DELETE FROM masts
WHERE mcc != 248;
ALTER TABLE masts ADD COLUMN masts_ID BIGSERIAL PRIMARY KEY;
ALTER TABLE masts ADD COLUMN geom geometry('POINT');
UPDATE masts
  set geom = 
    ST_SetSRID(ST_Point(CAST(lon as double precision), 
                        CAST(lat as double precision)), 4326);
CREATE INDEX geom_index ON masts USING GIST (geom);
ALTER TABLE masts ADD COLUMN geom_voronoi geometry('POLYGON');
"
dbGetQuery(con, q)
# q <- ("ALTER TABLE masts drop column geom_voronoi;
#       ALTER TABLE masts ADD COLUMN geom_voronoi geometry('POLYGON');")
# dbGetQuery(con, q)


# --- Voronoi Polygons must be calculated in R ---------------------------------
# library(rgeos)
library(sp)
library(rgdal)
library(magrittr)
q <- "SELECT masts_ID, ST_X(ST_Transform(geom, 3301)) as x,  
  ST_Y(ST_Transform(geom, 3301)) as y, cell, mcc, net, area from masts
;"
mast_points <- dbGetQuery(con, q)
coordinates(mast_points) <- ~x+y
proj4string(mast_points) <- CRS("+init=epsg:3301")

# --- Get the bounding box for Estonia and transform
SpatialPolygons(list(Polygons(list(Polygon(matrix(c(21.771851, 21.771851,  28.20989,  28.20989, 21.771851, 
                 57.509312,  59.685749,  59.685749, 57.509312, 57.509312), 
               ncol = 2))), ID = "1")), proj4string=CRS("+init=epsg:4326")) %>% 
  spTransform(CRS("+init=epsg:3301")) -> bbox
bbox_poly <- bbox@polygons[[1]]@Polygons[[1]]

# --- Do the Voronoi tesselation for every provider
for(net_ in 1:3){
  set <- mast_points[mast_points@data$net == net_, ]
  set <- set[order(set@data$masts_id), ]
  set@data$temp_id <- 1:length(set)
  set_unique <- set[!duplicated(set@coords), ]
  tesselation_t <- voronoipolygons(set_unique, bbox_poly)
  tesselation_t2 <- 
    SpatialPolygonsDataFrame(tesselation_t, 
                             data = data.frame(net = 
                                                 rep(net_, 
                                                     length(tesselation_t))))
  if(net_ == 1){
    tesselation <- tesselation_t2
  }else{
    n <- length(tesselation)
    tesselation <- rbind(spChFIDs(tesselation, 
                                  as.character(1:n)),
                         spChFIDs(tesselation_t2, 
                                  as.character((n + 1):
                                                 (n + length(tesselation_t2)))))
  }
}
tesselation <- spTransform(tesselation, CRS("+init=epsg:4326"))

# --- Show the tesselation on a map
library(leaflet)
temp <- tesselation
mp <- spTransform(mast_points, CRS("+init=epsg:4326"))

m <- leaflet() %>% addTiles(group = "OSM") %>%
  addPolygons(data = temp[temp@data$net == 1,], group = "EMT") %>%
  addCircles(data = mp[mp@data$net == 1,], group = "EMT") %>%
  addPolygons(data = temp[temp@data$net == 2,], group = "Elisa", color = "green") %>%
  addCircles(data = mp[mp@data$net == 2,], group = "Elisa", color = "green") %>%
  addPolygons(data = temp[temp@data$net == 3,], group = "Tele 2", color = "purple") %>%
  addCircles(data = mp[mp@data$net == 3,], group = "Tele 2", color = "purple") %>%
  addLayersControl(baseGroups = c("EMT", "Elisa", "Tele 2"))
m

# --- Upload the polygons back to the database
dbWriteSpatial(con, tesselation, tablename = "tesselation", replace = T)
# Test that the cell id is unique (within net and area)
q <- "SELECT net, area, cell, count(*) as anz
from masts 
group by net, area, cell
;"
print("Test that cell IDs are unique")
max(dbGetQuery(con, q)$anz) == 1
# Test that every (relevant) point is exactly in one polygon
q <- "SELECT a.net, a.area, a.cell, ST_X(a.geom) as x, ST_Y(a.geom) as y
	from masts a left join tesselation b
	on not ST_Contains(b.geom, a.geom) and a.net = b.net
	where a.cell <= 65535 and b.geom is null
  group by a.net, a.area, a.cell, a.geom
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
                  ST_Transform(a.geom, 3301)) and a.net = b.net
;
"
t <- dbGetQuery(con, q)


