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

q_gps_id <- "
ALTER TABLE gps ADD COLUMN gps_ID BIGSERIAL;
ALTER TABLE gps ADD COLUMN t_next TIMESTAMP WITH TIME ZONE;
ALTER TABLE gps ADD  COLUMN day SMALLINT;
ALTER TABLE gps ADD  COLUMN day_next SMALLINT;
UPDATE gps as a
  SET t_next = b.t_next,
    day = EXTRACT(DOY FROM a.t),
    day_next = b.day_next
  FROM (SELECT gps_id, lead(t, 1) OVER w as t_next,
           EXTRACT(DOY FROM lead(t, 1) OVER w) as day_next
        FROM gps
        WINDOW w AS (PARTITION BY uid ORDER BY t)
  ) b
  WHERE a. gps_id = b.gps_id;
ALTER TABLE gps ADD CONSTRAINT gps_pk PRIMARY KEY (gps_id);
CREATE INDEX gps_uid_t_tnext ON gps(uid, day, day_next);
CREATE INDEX gps_sp_index ON gps USING GIST (geom);
"
system.time(dbGetQuery(con, q_gps_id))
# 2016-06-02: 400 sekunden. (passage ab update...)

# ------------------------------------------------------------------------------
# --- Masts --------------------------------------------------------------------
# ------------------------------------------------------------------------------

source("02b_fix_masts.R")

# ------------------------------------------------------------------------------
# --- Fix the DCL Table --------------------------------------------------------
# ------------------------------------------------------------------------------

q <- "
ALTER TABLE device_cell_location
  ADD COLUMN cell numeric,
  ADD COLUMN area NUMERIC,
  ADD COLUMN mcc numeric,
  ADD COLUMN net numeric;
  ADD COLUMN id_dcl BIGSERIAL;
  ADD COLUMN t_next timestamp with time zone;
  ALTER TABLE device_cell_location ADD COLUMN day SMALLINT;
  ALTER TABLE device_cell_location ADD COLUMN day_next SMALLINT;
UPDATE device_cell_location
  SET cell = force_cast(cid),
  SET area = force_cast(lac),
  SET mcc = force_cast(substr(operator_numeric, 1, 3)),
  SET net = force_cast(substr(operator_numeric, 4, 2)),
  SET day = EXTRACT(DOY FROM t), 
  DROP COLUMN cid,
  DROP COLUMN lac,
  DROP COLUMN operator_numeric;
UPDATE device_cell_location as a
  SET t_next = b.t_next
  FROM (
    SELECT id_dcl, lead(t,1) OVER W as t_next, 
      EXTRACT(DOY FROM LEAD(t, 1) OVER w) as day_next
    FROM device_cell_location
    WINDOW w AS (PARTITION BY uid ORDER BY t ASC)
  ) b
  WHERE b.id_dcl = a.id_dcl;

ALTER TABLE device_cell_location ADD CONSTRAINT dcl_pk PRIMARY KEY (id_dcl);
ALTER TABLE device_cell_location ADD CONSTRAINT dcl_id_masts_fk FOREIGN KEY 
  (id_masts) REFERENCES masts(id_masts);
CREATE INDEX dcl_mast_connection ON device_cell_location (mcc, net, area, cell);
CREATE INDEX dcl_id_masts ON device_cell_location(id_masts);
CREATE INDEX dcl_uid_day_day_next ON device_cell_location(uid, day, day_next);
"
dbGetQuery(con, q)

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

# --- 

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
# --- Gaps ---------------------------------------------------------------------
# ------------------------------------------------------------------------------
d_heartbeat <- "'13 min'"
d_gps_t <- "'5 min'"
d_gps_s <- 500

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

/* Fill with GPS gaps */
INSERT INTO gaps
SELECT uid, t, t_next, 'gps' AS type
FROM
  (SELECT uid, t, t_next, 
    ST_DISTANCE(ST_TRANSFORM(geom, 3301), 
                ST_TRANSFORM(lead(geom, 1) OVER 
                  (PARTITION BY uid ORDER BY t ASC), 3301)) 
    AS dist
  FROM gps
  WHERE age(t_next, t) > ", d_gps_t, ") b
WHERE dist > ", d_gps_s, "

/* User Preferences Pause */
INSERT INTO gaps
SELECT uid, t, LEAST(t_next, t + '1 day'), 'pause' AS type
            FROM (SELECT uid, t, 
              lead(t, 1) OVER (PARTITION BY uid ORDER BY t ASC) AS t_next
            FROM user_prefs_pause) b

/*  */
")
dbGetQuery(con,q)


# --- mast info on dcl


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
ALTER TABLE device_cell_location ADD COLUMN id_masts BIGINT;
UPDATE device_cell_location AS z
set id_masts = d.id_masts
FROM 
	(SELECT a.id_dcl, 
    last_value(c.id_masts) over (PARTITION BY a.id_dcl 
                                 ORDER BY c.radio_level asc) as id_masts
	FROM
    device_cell_location a LEFT JOIN user_characteristics b
  ON a.uid = b.uid
  LEFT JOIN masts c
  ON a.mcc = c.mcc and a.net = c.net and a.area = c.area and a.cell = c.cell
  WHERE c.radio_level <= case when b.lte > 0.1 then 3 else 2 end) d
WHERE d.id_dcl = z.id_dcl;
;
ALTER TABLE device_cell_location 
  ADD CONSTRAINT device_cell_location_id_masts_fk 
  FOREIGN KEY (id_masts) 
  REFERENCES masts(id_masts);
"
system.time({dbGetQuery(con,q)})




