# This script takes .csv files in the "Sandkasten" directory and 
# puts them into the database.


source("00_defs_funcs.R")
connect(user = "burkhard", 
        pw = scan("nosafe.txt", what = "character"), 
        dbname = "burkhard",
        host = "localhost", 
        port = 5432)

dbGetQuery(con, "alter database burkhard set work_mem='2GB';");
dbGetQuery(con, "alter database burkhard set shared_buffers='2GB';");

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
sand_path <- "/project-data/userdata/sandkasten"
res <- sapply(names(col_headers), function(name_){
  import_csv(name_, col_headers, sand_path)
})

# ------------------------------------------------------------------------------
# --- Fix the GPS table --------------------------------------------------------
# ------------------------------------------------------------------------------

q_gps2 <- "
DROP TABLE IF EXISTS gps;
  SELECT
    CAST(uid as numeric) as uid,
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

q_id_gps <- "
ALTER TABLE gps ADD COLUMN id_gps BIGSERIAL;
ALTER TABLE gps ADD COLUMN t_next TIMESTAMP WITH TIME ZONE;
ALTER TABLE gps ADD  COLUMN day SMALLINT;
ALTER TABLE gps ADD  COLUMN day_next SMALLINT;
ALTER TABLE gps ADD COLUMN geom_next GEOMETRY(POINT);
UPDATE gps as a
  SET t_next = b.t_next,
    day = EXTRACT(DOY FROM a.t),
    day_next = b.day_next,
    geom_next = b.geom_next
  FROM (SELECT id_gps, lead(t, 1) OVER w as t_next,
           EXTRACT(DOY FROM lead(t, 1) OVER w) as day_next,
           lead(geom, 1) OVER w AS geom_next
        FROM gps
        WINDOW w AS (PARTITION BY uid ORDER BY t)
  ) b
  WHERE a. id_gps = b.id_gps;
ALTER TABLE gps ADD CONSTRAINT gps_pk PRIMARY KEY (id_gps);
CREATE INDEX gps_uid_day_day_next ON gps(uid, day, day_next);
CREATE INDEX gps_sp_index ON gps USING GIST (geom);
CREATE INDEX gps_uid_t_t_next ON gps (uid, t, t_next);
"
system.time(dbGetQuery(con, q_id_gps))
# 2016-06-02: 400 sekunden. (passage ab update...)

# ------------------------------------------------------------------------------
# --- Masts --------------------------------------------------------------------
# ------------------------------------------------------------------------------

source("02b_fix_masts.R")

# ------------------------------------------------------------------------------
# --- Fix the DCL Table --------------------------------------------------------
# ------------------------------------------------------------------------------

# WARNING !!! I expcicitly eclude cell = -1 and area != -1 from the data
#             This might need to be revisited at some later stage.
q <- "
DELETE FROM device_cell_location WHERE cid = '-1' AND lac != '-1';
ALTER TABLE device_cell_location
  ADD COLUMN cell numeric,
  ADD COLUMN area NUMERIC,
  ADD COLUMN mcc numeric,
  ADD COLUMN net numeric,
  ADD COLUMN id_dcl BIGSERIAL,
  ADD COLUMN t_next timestamp with time zone;
  ALTER TABLE device_cell_location ADD COLUMN day SMALLINT;
  ALTER TABLE device_cell_location ADD COLUMN day_next SMALLINT;
UPDATE device_cell_location
  SET cell = force_cast(cid),
    area = force_cast(lac),
    mcc = force_cast(substr(operator_numeric, 1, 3)),
    net = force_cast(substr(operator_numeric, 4, 2)),
    day = EXTRACT(DOY FROM t);
ALTER TABLE device_cell_location DROP COLUMN cid;
ALTER TABLE device_cell_location DROP COLUMN lac;
ALTER TABLE device_cell_location DROP COLUMN operator_numeric;
UPDATE device_cell_location as a
  SET t_next = b.t_next, day_next = b.day_next
  FROM (
    SELECT id_dcl, lead(t,1) OVER W as t_next,
      EXTRACT(DOY FROM LEAD(t, 1) OVER w) as day_next
    FROM device_cell_location
    WINDOW w AS (PARTITION BY uid ORDER BY t ASC)
  ) b
  WHERE b.id_dcl = a.id_dcl;

ALTER TABLE device_cell_location ADD CONSTRAINT dcl_pk PRIMARY KEY (id_dcl);
CREATE INDEX dcl_mast_connection ON device_cell_location (mcc, net, area, cell);
CREATE INDEX dcl_uid_day_day_next ON device_cell_location(uid, day, day_next);
CREATE INDEX dcl_uid_time_time_next ON device_cell_location(uid, t, t_next);
"
system.time(dbGetQuery(con,q))
# 2016-06-13: 357 (erster teil mit updates) + 75 (indices)

# ------------------------------------------------------------------------------
# --- User Preferences ---------------------------------------------------------
# ------------------------------------------------------------------------------

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

# ------------------------------------------------------------------------------
# --- Add masts-info to DCL ----------------------------------------------------
# ------------------------------------------------------------------------------

# The 4-info-identifier is not unique. We assign the mast with the highest
# radio level available to the user (UMTS or LTE)
# rule: above 10% LTE connections means the phone is LTE-capable.

q <- "
ALTER TABLE device_cell_location ADD COLUMN id_masts BIGINT;
UPDATE device_cell_location AS z
set id_masts = d.id_masts
FROM 
  (SELECT 
    a.id_dcl, last_value(c.id_masts) over (PARTITION BY a.id_dcl 
    ORDER BY c.radio_level ASC) as id_masts
  FROM
    device_cell_location a 
    JOIN user_characteristics b
      ON a.uid = b.uid
    JOIN masts c
      ON a.mcc = c.mcc and a.net = c.net and a.area = c.area and a.cell = c.cell
      WHERE c.radio_level <= case when b.lte > 0.1 then 3 else 2 end) d
WHERE d.id_dcl = z.id_dcl;
;
ALTER TABLE device_cell_location 
  ADD CONSTRAINT device_cell_location_id_masts_fk 
  FOREIGN KEY (id_masts) 
  REFERENCES masts(id_masts);
CREATE INDEX dcl_id_masts ON device_cell_location(id_masts);
"
system.time(dbGetQuery(con,q))
# 2016-06-13: 468 secounds.
# 2016-06-13: 391 secounds. (nach ausschluss der -1/>-1 punkte)

# ------------------------------------------------------------------------------
# --- Gather CDR data ----------------------------------------------------------
# ------------------------------------------------------------------------------
q <- "
DROP TABLE IF EXISTS all_cdr CASCADE;
CREATE TABLE all_cdr as
SELECT a.*, b.id_dcl, b.id_masts, b.net from (
    SELECT uid,  to_timestamp(time_start) as t_start, 
      to_timestamp(time_end) as t_end,
      age(to_timestamp(time_end), to_timestamp(time_start)) as duration, 
      eventtype
    FROM call_incoming_answered
  UNION ALL
    SELECT uid,  t as t_start, NULL as t_end, NULL as duration, eventtype
    FROM call_incoming_missed
  UNION ALL
    SELECT uid,  to_timestamp(time_start) as t_start, 
      to_timestamp(time_end) as t_end,
      age(to_timestamp(time_end), to_timestamp(time_start)) as duration, 
      eventtype
    FROM call_outgoing
  UNION ALL
    SELECT uid,  t as t_start, NULL as t_end, NULL as duration, eventtype
    FROM sms_incoming
  UNION ALL
    SELECT uid,  t as t_start, NULL as t_end, NULL as duration, eventtype
    FROM sms_outgoing ) a 
JOIN device_cell_location b
ON a.uid = b.uid and a.t_start >= b.t and (a.t_start < b.t_next)
;
ALTER TABLE all_cdr ADD COLUMN id_all_cdr BIGSERIAL PRIMARY KEY;
ALTER TABLE all_cdr ALTER COLUMN net TYPE INTEGER;
"
t <- dbGetQuery(con, q)
# not all fixes have mast_ids (because not all masts are in opencellid)
#   however, only 4% (15k) are missing so we leave it for now.
# q <- "
# EXPLAIN (ANALYZE, BUFFERS, VERBOSE) UPDATE ALL_CDR as a
# SET id_masts = e.id_masts, proxy_mast = true
# FROM 
#  (SELECT b.id_all_cdr, d.id_masts
#   FROM all_cdr b 
#     JOIN gps c ON b.uid = c.uid AND 
#       b.t_start BETWEEN c.t AND c.t_next
#     JOIN masts d ON d.mcc = 248 AND 
#       ST_WITHIN(c.geom, d.geom_voronoi) AND
#       d.net = b.net
# 	WHERE b.id_masts IS NULL) e
# WHERE a.id_masts IS NULL and a.id_all_cdr = e.id_all_cdr
# ;"
# t <- dbGetQuery(con, q)

# ------------------------------------------------------------------------------
# --- Gaps Table (not gps) -----------------------------------------------------
# ------------------------------------------------------------------------------
d_heartbeat <- "'13 min'"

q <- paste0("
DROP TABLE IF EXISTS gaps;
CREATE TABLE gaps (
  uid numeric,
  t_start TIMESTAMP WITH TIME ZONE,
  t_end TIMESTAMP WITH TIME ZONE,
  type VARCHAR (20)
);

/* Fill with Heartbeat gaps */
INSERT INTO gaps
SELECT uid, t, t_next, 'heartbeat' AS type
FROM (SELECT uid, t, 
        lead(t, 1) OVER (PARTITION BY uid ORDER BY t ASC) AS t_next
      FROM app_heartbeat) b
WHERE age(b.t_next, b.t) > ", d_heartbeat, ";

/* User Preferences Pause */
INSERT INTO gaps
SELECT uid, t, LEAST(t_next, t + '1 day'), 'pause' AS type
            FROM (SELECT uid, t, 
              lead(t, 1) OVER (PARTITION BY uid ORDER BY t ASC) AS t_next
            FROM user_prefs_pause) b;

/* Index on uid and time */
CREATE INDEX gaps_uid_time ON gaps (uid, t_start, t_end);
")
dbGetQuery(con,q)

# ------------------------------------------------------------------------------
# --- Gaps on GPS --------------------------------------------------------------
# ------------------------------------------------------------------------------
d_gps_t <- "'5 min'"
d_gps_s <- 500
d_gps_t_large <- "'2 days'"
# 2: spatial and distance threshold
# 3: user pause
# 5: heartbeat
# 7: ausserhalb estlands
# 9: temporal threshold for really big gaps

q <- paste0("
ALTER TABLE gps ADD COLUMN flag_problem INTEGER;
UPDATE gps SET flag_problem = 1;
UPDATE gps SET flag_problem = flag_problem * 2
    WHERE AGE(t_next, t) > ", d_gps_t, " AND 
    ST_DISTANCE(ST_TRANSFORM(geom, 3301), ST_TRANSFORM(geom_next, 3301)) > ",
            d_gps_s, ";

UPDATE gps as a 
  SET flag_problem = flag_problem * 3
  FROM (
    SELECT b.id_gps FROM gps b JOIN gaps c
    ON b.uid = c.uid and c.type = 'pause' and b.t <= c.t_start and
      b.t_next >= c.t_end
  ) d 
  WHERE a.id_gps = d.id_gps;

UPDATE gps AS a
  SET flag_problem = flag_problem * 5
  FROM (
    SELECT b.id_gps FROM gps b JOIN gaps c
    ON b.uid = c.uid and c.type = 'heartbeat' and 
      b.t >= (c.t_start - INTERVAL '60 seconds') and
      b.t <= (c.t_end - INTERVAL '5 minutes') and
      age(b.t_next, b.t) > '10 minutes'
  ) d 
  WHERE a.id_gps = d.id_gps;
")
dbGetQuery(con,q)

library(sp)
library(rgeos)
library(magrittr)
EE <- readRDS("/project-data/userdata/rawdata/EST_adm0.rds")
mainland <- which.max(sapply(EE@polygons[[1]]@Polygons, 
                             function(p) nrow(p@coords)))
EE@polygons[[1]]@Polygons[[mainland]] %>% list %>% Polygons(ID = 1) %>% list %>%
  SpatialPolygons(proj4string = CRS("+init=epsg:4326")) %>% 
  spTransform(CRS("+init=epsg:3301")) %>%
  gSimplify(tol = 3000) %>%  # Simplifies the buffering afterwards
  gBuffer(width = 10000) %>% 
  gSimplify(tol = 3000) %>% 
  spTransform(CRS("+init=epsg:4326"))-> EEm
# leaflet() %>% addTiles %>% addPolygons(data = EEm)
dbWriteSpatial(con, EEm, tablename = "geog_shapes")

q <- paste0("
UPDATE gps as a SET flag_problem = flag_problem * 7
FROM geog_shapes b
WHERE NOT ST_CONTAINS(b.geom, a.geom);

UPDATE gps SET flag_problem = flag_problem * 9
    WHERE AGE(t_next, t) > ", d_gps_t_large, ";
ALTER TABLE gps ADD COLUMN problem_proximity SMALLINT;

UPDATE gps as a SET problem_proximity = b.problem_proximity 
  FROM (
    SELECT id_gps, 
      case when lead(flag_problem, 1) over w > 1 then 2 else 1 end * 
      case when lag(flag_problem, 1) over w > 1 then 3 else 1 end as 
      problem_proximity
    FROM gps
    WINDOW w AS (PARTITION BY uid ORDER BY t ASC)
  ) b
  WHERE a.id_gps = b.id_gps;
")
dbGetQuery(con,q)

