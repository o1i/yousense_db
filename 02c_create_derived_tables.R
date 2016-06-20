source("00_defs_funcs.R")
connect(user = "burkhard", 
        pw = scan("nosafe.txt", what = "character"), 
        dbname = "burkhard",
        host = "localhost", 
        port = 5432)

# ------------------------------------------------------------------------------
# --- GPS + GSM  ---------------------------------------------------------------
# ------------------------------------------------------------------------------

# --- Create the gps_gsm table
# Due to me not being able to intelligently index a partition of the time
#   performing the whole query in one go has an incredible runtime.
# Therefore I split the query into one part that can use the indexes on days
#   and three other parts that are "small" and do not take a lot of time.
q <- "
ANALYZE gps;
ANALYZE device_cell_location;
CREATE TABLE gps_gsm AS

/* Part one: large but with indexes */
SELECT a.uid, a.id_gps, b.id_dcl, c.id_masts, a.geom AS geom_gps,
GREATEST(a.t, b.t) AS t_start,
LEAST(a.t_next, b.t_next) AS t_end, b.net, c.geom AS geom_mast,
c.neighbour_avg_dist, c.neighbour_max_dist, c.neighbour_area, c.samples,
c.radio_level,
ST_DISTANCE(ST_TRANSFORM(a.geom, 3301), ST_TRANSFORM(c.geom, 3301)) AS dist,
a.problem_proximity, a.flag_problem
FROM
gps a
JOIN device_cell_location b
ON a.uid = b.uid and 
b.t <= a.t_next and b.t_next > a.t and a.day = b.day and a.day = a.day_next and b.day = b.day_next and
b.t_next is not null and a.t_next is not null
JOIN masts c
ON b.id_masts = c.id_masts
WHERE c.mcc = 248
UNION ALL

/* Part two: day overlap on gps*/
SELECT a.uid, a.id_gps, b.id_dcl, c.id_masts, a.geom AS geom_gps,
GREATEST(a.t, b.t) AS t_start,
LEAST(a.t_next, b.t_next) AS t_end, b.net, c.geom AS geom_mast,
c.neighbour_avg_dist, c.neighbour_max_dist, c.neighbour_area, c.samples,
c.radio_level,
ST_DISTANCE(ST_TRANSFORM(a.geom, 3301), ST_TRANSFORM(c.geom, 3301)) AS dist,
a.problem_proximity, a.flag_problem
FROM
gps a
JOIN device_cell_location b
ON a.uid = b.uid and 
b.t <= a.t_next and b.t_next > a.t and a.day != a.day_next and b.day = b.day_next and
b.t_next is not null and a.t_next is not null
JOIN masts c
ON b.id_masts = c.id_masts
WHERE c.mcc = 248
UNION ALL

/* Part three: day overlap on dcl*/
SELECT a.uid, a.id_gps, b.id_dcl, c.id_masts, a.geom AS geom_gps,
GREATEST(a.t, b.t) AS t_start,
LEAST(a.t_next, b.t_next) AS t_end, b.net, c.geom AS geom_mast,
c.neighbour_avg_dist, c.neighbour_max_dist, c.neighbour_area, c.samples,
c.radio_level,
ST_DISTANCE(ST_TRANSFORM(a.geom, 3301), ST_TRANSFORM(c.geom, 3301)) AS dist,
a.problem_proximity, a.flag_problem
FROM
gps a
JOIN device_cell_location b
ON a.uid = b.uid and 
b.t <= a.t_next and b.t_next > a.t and a.day = a.day_next and b.day != b.day_next and
b.t_next is not null and a.t_next is not null
JOIN masts c
ON b.id_masts = c.id_masts
WHERE c.mcc = 248
UNION ALL

/* Part four: day overlap on gps and dcl*/
SELECT a.uid, a.id_gps, b.id_dcl, c.id_masts, a.geom AS geom_gps,
GREATEST(a.t, b.t) AS t_start,
LEAST(a.t_next, b.t_next) AS t_end, b.net, c.geom AS geom_mast,
c.neighbour_avg_dist, c.neighbour_max_dist, c.neighbour_area, c.samples,
c.radio_level,
ST_DISTANCE(ST_TRANSFORM(a.geom, 3301), ST_TRANSFORM(c.geom, 3301)) AS dist,
a.problem_proximity, a.flag_problem
FROM
gps a
JOIN device_cell_location b
ON a.uid = b.uid and 
b.t <= a.t_next and b.t_next > a.t and a.day != a.day_next and b.day != b.day_next and
b.t_next is not null and a.t_next is not null
JOIN masts c
ON b.id_masts = c.id_masts
WHERE c.mcc = 248
;"
Sys.time()
system.time(t <- dbGetQuery(con,q))
t
# Over night to 2016-06-01: 9259, i.e. 2.6 h
# 2016-06-03 after some optimisation: 7850, i.e. 2.1h
# 2016-06-10 1h when indexing on days
# Auf neuem Server und auf 4 Teile aufgeteilt: 648, i.e. ca 10 minuten.
# 2016-06-16 755
# 2016-06-16 658
# 2016-06-16 656

q <- "
CREATE INDEX gps_gsm_uid ON gps_gsm(uid);
CREATE INDEX gps_gsm_time ON gps_gsm(t_start, t_end);
ALTER TABLE gps_gsm ADD CONSTRAINT gps_gsm_ids PRIMARY KEY (id_masts,
id_gps, t_start, t_end);
ANALYZE gps_gsm;
"
Sys.time()
system.time(t <- dbGetQuery(con,q))
dbGetQuery(con,"VACUUM gps_gsm;")

# ------------------------------------------------------------------------------
# --- Segmentation  ------------------------------------------------------------
# ------------------------------------------------------------------------------
library(parallel)

dbGetQuery(con, "CREATE TABLE segments(
uid numeric,
segment INTEGER,
id_gps bigint,
id_gps_end bigint,
t_start timestamp with time zone,
t_end timestamp with time zone,
stop BOOLEAN,
x_mean FLOAT,
y_mean FLOAT,
x_start FLOAT,
x_end FLOAT,
y_start FLOAT,
y_end FLOAT,
valid_start BOOLEAN,
valid_end BOOLEAN
           );")
users <- sort(dbGetQuery(con, 
                         "SELECT distinct uid from user_characteristics")[, 1])

sapply(users, function(user_){
  print(user_)
  q <- paste0("
SELECT t, ST_X(geom) as x, ST_Y(geom) as y, flag_problem, id_gps
FROM gps
WHERE uid = ", user_, "
ORDER BY t;")
  daten <- dbGetQuery(con, q)
  
  daten_segmentiert <- naive_segmentation(daten, 
                                          dt = 5, 
                                          dsp = 100, 
                                          dt_break = 180, 
                                          dt_short = 100, 
                                          ds_short = 100,
                                          problem = "flag_problem")
  segmente <- naive_simplification(daten_segmentiert, 100)
  segmente$uid <- user_
  segmente <- segmente[, c("uid", "segment", "id_gps", "id_gps_end", 
                         "t_start", "t_end", "stop", "x_mean", "y_mean",
                         "x_start", "x_end", "y_start", "y_end",
                         "valid_start", "valid_end")]
  
  dbGetQuery(con, "SET TIME ZONE 'UTC';")
  Sys.setenv(TZ='GMT')
  dbWriteTable(con, "segments", 
               value = segmente, append = TRUE, row.names = FALSE)
})

dbGetQuery(con, "
ALTER TABLE segments ADD COLUMN day SMALLINT;
ALTER TABLE segments ADD COLUMN day_next SMALLINT;
UPDATE segments SET
  day = extract(DOY FROM t_start),
  day_next = extract(DOY FROM t_end);
ALTER TABLE segments ADD CONSTRAINT segmente_pk 
           PRIMARY KEY (uid,segment);
ALTER TABLE segments ADD COLUMN geom_start GEOMETRY(POINT);
ALTER TABLE segments ADD COLUMN geom_mean  GEOMETRY(POINT);
ALTER TABLE segments ADD COLUMN geom_end   GEOMETRY(POINT);
UPDATE segments SET
  geom_start = ST_SETSRID(ST_POINT(x_start, y_start), 4326),
  geom_mean  = ST_SETSRID(ST_POINT(x_mean , y_mean) , 4326),
  geom_end   = ST_SETSRID(ST_POINT(x_end  , y_end)  , 4326);
")













