# This script takes .csv files in the "Sandkasten" directory and 
# puts them into the database.



library("RPostgreSQL")

connect(user = "burkhard", 
        pw = scan("nosafe.txt", what = "character"), 
        dbname = "burkhard",
        host = "localhost", 
        port = 5432)

dbGetQuery(con, "alter database burkhard set work_mem='2GB'");
dbGetQuery(con, "alter database burkhard set shared_buffers='2GB'");

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

# --- Fix the GPS table --------------------------------------------------------
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
  ORDER BY uid, t
  ;"
Sys.time()
system.time(dbGetQuery(con, q_gps2))
# 2016-06-02: Should take about 40 seconds!

q_gps_id <- "
ALTER TABLE gps ADD COLUMN gps_ID BIGSERIAL PRIMARY KEY;
CREATE INDEX gps_sp_index ON gps USING GIST (geom);
ALTER TABLE gps ADD  COLUMN t_next TIMESTAMP WITH TIME ZONE;
UPDATE gps as a
  SET t_next = b.t_next
  FROM (SELECT gps_id, lead(t, 1) OVER (PARTITION BY uid ORDER BY t) as t_next
        FROM gps) b
  WHERE a. gps_id = b.gps_id;
CREATE INDEX gps_time ON gps (t, t_next);
CREATE INDEX gps_uid_t_tnext ON gps(uid, t, t_next);
"
system.time(dbGetQuery(con, q_gps_id))
# 2016-06-02: 400 sekunden. (passage ab update...)

# --- Device cell location -----------------------------------------------------

q <- "
ALTER TABLE device_cell_location
  ADD COLUMN cell numeric,
  ADD COLUMN area NUMERIC,
  ADD COLUMN mcc numeric,
  ADD COLUMN net numeric;
  ADD COLUMN id_dcl BIGSERIAL PRIMARY KEY;
  ADD COLUMN t_next timestamp with time zone;
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
UPDATE device_cell_location as a
  SET t_next = b.t_next
  FROM (
    SELECT id_dcl, lead(t,1) OVER (PARTITION BY uid ORDER BY t ASC) as t_next
    FROM device_cell_location
  ) b
  WHERE b.id_dcl = a.id_dcl;

CREATE INDEX device_cell_location_idx_mast_connection ON device_cell_location 
(mcc, net, area, cell);
CREATE INDEX device_cell_location_time ON device_cell_location (t, t_next);
CREATE INDEX dcl_t_next ON device_cell_location(t_next);
CREATE INDEX device_cell_location_uid_t_tnext ON 
  device_cell_location (uid, t, t_next);
ALTER TABLE device_cell_location ADD CONSTRAINT dcl_masts_id_fk FOREIGN KEY 
  (masts_id) REFERENCES masts(masts_id);
CREATE INDEX device_cell_location_masts_id ON device_cell_location(masts_id);
"
systemdbGetQuery(con, q)


# --- Create CDR summary table -------------------------------------------------
q <- "
DROP TABLE IF EXISTS all_cdr CASCADE;
CREATE TABLE all_cdr as
SELECT a.*, b.masts_id, b.net from (
  SELECT uid,  to_timestamp(time_start) as t_start, 
    to_timestamp(time_end) as t_end,
    age(to_timestamp(time_end), to_timestamp(time_start)) as duration, eventtype
  FROM call_incoming_answered
  UNION ALL
  SELECT uid,  t as t_start, NULL as t_end, NULL as duration, eventtype
  FROM call_incoming_missed
  UNION ALL
  SELECT uid,  to_timestamp(time_start) as t_start, 
    to_timestamp(time_end) as t_end,
    age(to_timestamp(time_end), to_timestamp(time_start)) as duration, eventtype
  FROM call_outgoing
  UNION ALL
  SELECT uid,  t as t_start, NULL as t_end, NULL as duration, eventtype
  FROM sms_incoming
  UNION ALL
  SELECT uid,  t as t_start, NULL as t_end, NULL as duration, eventtype
  FROM sms_outgoing ) a join device_cell_location b
ON a.uid = b.uid and a.t_start >= b.t and (a.t_start < b.t_next)
;
ALTER TABLE all_cdr ADD COLUMN id_all_cdr BIGSERIAL PRIMARY KEY;
ALTER TABLE all_cdr ADD COLUMN proxy_mast BOOLEAN;
"
t <- dbGetQuery(con, q)
q <- "
EXPLAIN (ANALYZE, BUFFERS, VERBOSE) UPDATE ALL_CDR as a
SET masts_id = e.masts_id, proxy_mast = true
FROM 
 (SELECT b.id_all_cdr, d.masts_id
  FROM all_cdr b 
    JOIN gps c ON b.uid = c.uid AND 
      b.t_start BETWEEN c.t AND c.t_next
    JOIN masts d ON d.mcc = 248 AND 
      ST_WITHIN(c.geom, d.geom_voronoi) AND
      d.net = b.net
	WHERE b.masts_id IS NULL) e
WHERE a.masts_id IS NULL and a.id_all_cdr = e.id_all_cdr
;"
t <- dbGetQuery(con, q)

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
ALTER TABLE masts ADD COLUMN radio_level numeric;
UPDATE masts
  set geom = 
    ST_SetSRID(ST_Point(CAST(lon as double precision), 
                        CAST(lat as double precision)), 4326);
UPDATE masts
  SET radio_level = case when radio = 'LTE' then 3 else 
    case when radio = 'UMTS' then 2 else 1 end end;
CREATE INDEX geom_index ON masts USING GIST (geom);
ALTER TABLE masts ADD COLUMN geom_voronoi geometry('POLYGON');
UPDATE masts SET radio = trim(both from radio);
CREATE INDEX masts_idx_geom_voronoi ON masts USING GIST (geom_voronoi);
CREATE INDEX masts_idx_mast_connection ON masts (mcc, net, area, cell);
CREATE INDEX masts_id_partial ON masts(masts_id) WHERE mcc = 248;
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
ALTER TABLE MASTS ADD COLUMN geom_proj geometry(point);
ALTER TABLE MASTS ADD COLUMN geom_voronoi_proj geometry(polygon);
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
    (SELECT a.masts_id, 
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
    GROUP BY a.masts_id
    ) m2
  WHERE 
    m1.masts_id = m2.masts_id and 
    m1.mcc = 248
  ;
"
dbGetQuery(con,q)

# --- Next we need additions to the GPS table ----------------------------------
# It has added information on 
#   the status of the phone (service level)
#   the connected masts (interpretable if the status is not 3)
#   the closest mast of the same level as the connected mast
# Also it creates a new record if any of the added information changes.
# As a last information, the time to the next gps-fix is provided
# Also, exclude points that do not lie within estonia.

# One source of error can be the fact that the system clock can be messed up.
# This has hardly any influence, but ideally would be considered. 
# select uid, max(t2 - t) as timediff, 
#   sum(case when t2 - t > '2 hours' then 1 else 0 end) as anz,
#   avg(case when t2 - t > '2 hours' then 1 else 0 end) as avg 
#   from gps group by uid order by avg desc;
# Simply having the wrong time in and of itself would not be that much of a big
# deal (although the weekdays/time schedules could get messed up)
# The problem comes from the fact when the same date/time combination gets
# Overwritten multiple times. The result would be implausible jumps between
#   two locations.
# As the problem is "small" I choose to ignore it for the time being.
# --> Always use t (even in the gps case where I would have the actual time)
# --> Note the timediff at every point, for future use.



# --- Status -------------------------------------------------------------------
# Create a table that contains the relevant gps information and adds
# The service status at each fix.
q <- "
DROP TABLE IF EXISTS gps_state CASCADE;
DROP TABLE IF EXISTS temp;
CREATE TABLE temp as 
  SELECT uid, t, lead(t, 1) OVER w as t_next, state
  FROM device_cell_service
  WINDOW w AS (PARTITION BY uid ORDER BY t);
CREATE INDEX temp_idx ON temp (uid);
CREATE INDEX temp_idx_t ON temp (t);
CREATE INDEX temp_idx_tnext ON temp (t_next);
CREATE INDEX temp_idx_test ON temp(uid, t, t_next);

CREATE TABLE gps_state as 
SELECT a.uid::numeric, a.gps_id, a.geom, greatest(a.t, b.t) as t_start, least(a.t_next, b.t_next) as t_end, 
  b.state 
FROM gps a left join 
  temp b
ON b.t < a.t_next and b.t_next > a.t and
a.uid = b.uid;
ALTER TABLE gps_state ADD CONSTRAINT gps_state_fk FOREIGN KEY (gps_id) 
  REFERENCES gps(gps_id) ON DELETE CASCADE ON UPDATE CASCADE;
CREATE INDEX gps_state_idx_gps_id ON gps_state(gps_id);
CREATE INDEX gps_state_uid ON gps_state(uid);
CREATE INDEX gps_state_time ON gps_state(t_start, t_end);
CREATE INDEX gps_state_uid_time ON gps_state (uid, t_start, t_end);
CREATE INDEX gps_state_uid_t_end ON gps_state (t_end);
"
Sys.time(); system.time(dbGetQuery(con,q))
# 2016-05-31: elapsed = 1010
# 2016-05-31: elapsed = 980
# 2016-06-02: 971 (bis alter table gps_state) +  37 (rest)

# --- User-Präferenzen ---------------------------------------------------------
# Starts the table with user preferences. Here the proportion of connections
#   To LTE antennae. (choice: > 10% means LTE-Capability)
q <- "
DROP TABLE IF EXISTS user_characteristics;
CREATE TABLE user_characteristics as 
SELECT uid, avg(case when b.radio = 'LTE' then 1 else 0 end) as lte
FROM device_cell_location a left join masts b
ON a.mcc = b.mcc and a.net = b.net and a.area = b.area and
  a.cell = b.cell
GROUP BY uid;
"
dbGetQuery(con,q)

# --- Masten -------------------------------------------------------------------

# --- Prestep: add level to device_cell_location
# muss überarbeitet werden.
# Somehow this takes 45 minutes. Try to optimise for next time
system.time({
q <- "
DROP TABLE IF EXISTS temp CASCADE;
CREATE TABLE temp AS 
SELECT a.id_dcl, 
  max(c.radio_level) as radio_level
FROM 
  device_cell_location a LEFT JOIN
  user_characteristics b
    ON a.uid = b.uid 
  LEFT JOIN masts c
    ON
    c.mcc = a.mcc and 
    c.net = a.net and 
    c.area = a.area and 
    c.cell = a.cell and 
    c.radio_level <= case when b.lte > 0.1 then 3 else 2 end
GROUP BY a.id_dcl
;
CREATE INDEX temp_idx ON temp(id_dcl);
ALTER TABLE device_cell_location ADD COLUMN radio_level numeric;
UPDATE device_cell_location as a
SET radio_level = b.radio_level
FROM temp b
WHERE a.id_dcl = b.id_dcl;
"
dbGetQuery(con,q)
})

# --- Add the mast_id to the connection table
q <- "
ALTER TABLE device_cell_location ADD COLUMN masts_id BIGINT;
UPDATE device_cell_location AS z
set masts_id = d.masts_id
FROM 
	(SELECT a.id_dcl, 
    last_value(c.masts_id) over (PARTITION BY a.id_dcl 
                                 ORDER BY c.radio_level asc) as masts_id
	FROM
    device_cell_location a LEFT JOIN user_characteristics b
  ON a.uid = b.uid
  LEFT JOIN masts c
  ON a.mcc = c.mcc and a.net = c.net and a.area = c.area and a.cell = c.cell
  WHERE c.radio_level <= case when b.lte > 0.1 then 3 else 2 end) d
WHERE d.id_dcl = z.id_dcl;
;
ALTER TABLE device_cell_location 
  ADD CONSTRAINT device_cell_location_masts_id_fk 
  FOREIGN KEY (masts_id) 
  REFERENCES masts(masts_id);
"
system.time({dbGetQuery(con,q)})




