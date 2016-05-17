# This script takes .csv files in the "Sandkasten" directory and 
# puts them into the database.



library("RPostgreSQL")

try(disconnect(), silent = T)
connect(user = "burkhard", 
        pw = scan("nosafe.txt", what = "character"), 
        dbname = "burkhard",
        host = "localhost", 
        port = 5432)

# --- Define custom function ---------------------------------------------------
q <- "
CREATE OR REPLACE FUNCTION to_timestamp(text) RETURNS timestamp with time zone
AS 'select to_timestamp($1::double precision / 1000);'
LANGUAGE SQL
IMMUTABLE
RETURNS NULL ON NULL INPUT;"
dbGetQuery(con, q)

q <- "
create or replace function force_cast(text) returns integer as $$
  begin
return cast($1 as integer);
exception
when invalid_text_representation then
return NULL;
end;
$$ language plpgsql immutable;"
dbGetQuery(con, q)

# --- Copy all the .csv files into the database --------------------------------
res <- sapply(names(col_headers), function(name_){
  import_csv(name_, col_headers, sand_path)
})

# --- Modify tables where necessary --------------------------------------------
q_gps2 <- "
 DROP TABLE IF EXISTS gps;
  SELECT
    CAST(uid as integer) as uid,
    t,
    TO_TIMESTAMP(time2) as t2,
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

q <- "
ALTER TABLE device_cell_location
  ADD COLUMN cell numeric,
  ADD COLUMN area NUMERIC,
  ADD COLUMN mcc numeric,
  ADD COLUMN net numeric;
UPDATE device_cell_location
  SET cell = force_cast(cid);
UPDATE device_cell_location
  SET area = force_cast(lac);
UPDATE device_cell_location
  SET mcc = force_cast(substr(operator_numeric, 1, 3));
UPDATE device_cell_location
  SET net = force_cast(substr(operator_numeric, 4, 2));
ALTER TABLE device_cell_location
  DROP COLUMN cid,
  DROP COLUMN lac,
  DROP COLUMN operator_numeric;

CREATE INDEX device_cell_location_idx_mcc ON device_cell_location (mcc);
CREATE INDEX device_cell_location_idx_net ON device_cell_location (net);
CREATE INDEX device_cell_location_idx_area ON device_cell_location (area);
CREATE INDEX device_cell_location_idx_cell ON device_cell_location (cell);
"
dbGetQuery(con, q)


# --- Create CDR summary table -------------------------------------------------
q <- "
CREATE OR REPLACE VIEW all_cdr as
SELECT uid,  t, eventtype
FROM call_incoming_answered
UNION ALL
SELECT uid,  t, eventtype
FROM call_incoming_missed
UNION ALL
SELECT uid,  t, eventtype
FROM call_outgoing
UNION ALL
SELECT uid,  t, eventtype
FROM sms_incoming
UNION ALL
SELECT uid,  t, eventtype
FROM sms_outgoing
;"
t <- dbGetQuery(con, q)

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
COPY masts
FROM '/project-data/userdata/rawdata/cell_towers.csv'
CSV HEADER;
DELETE FROM masts
WHERE mcc not in (248, 228);
ALTER TABLE masts ADD COLUMN masts_ID BIGSERIAL PRIMARY KEY;
ALTER TABLE masts ADD COLUMN geom geometry('POINT');
UPDATE masts
  set geom = 
    ST_SetSRID(ST_Point(CAST(lon as double precision), 
                        CAST(lat as double precision)), 4326);
CREATE INDEX geom_index ON masts USING GIST (geom);
ALTER TABLE masts ADD COLUMN geom_voronoi geometry('POLYGON');
UPDATE masts SET radio = trim(both from radio);
CREATE INDEX masts_idx_geom_voronoi ON masts USING GIST (geom_voronoi);
"
dbGetQuery(con, q)


# --- Voronoi Polygons must be calculated in R ---------------------------------
# library(rgeos)
library(sp)
library(rgdal)
library(magrittr)
q <- "SELECT masts_ID, radio,
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
      set <- set[order(set@data$masts_id), ]
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
                  ST_Transform(a.geom, 3301)) and a.mcc = b.mcc and 
                  a.net = b.net and a.radio = b.radio and a
;
"
t <- dbGetQuery(con, q)





