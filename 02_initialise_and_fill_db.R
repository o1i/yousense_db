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
"
dbGetQuery(con, q_gps_id)

try(disconnect(), silent = T)
