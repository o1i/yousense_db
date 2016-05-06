# This script extracts the "significant stop" parts of the trajectories

q <- "
  CREATE TABLE POT_STOPS as 
  SELECT 
    a.uid, 
    a.t,
    a.geom
  FROM
    gps a left join gps b
  ON
    ABS(extract(epoch from a.t-b.t)) <= 130 and
    a.uid = b.uid
  GROUP BY
    a.gps_id
  HAVING
    sum(case when ST_DISTANCE(a.geom, b.geom) <  100 then 1 else 0 end) > 3 and
    sum(case when ST_DISTANCE(a.geom, b.geom) >= 100 then 1 else 0 end) = 0
"
dbGetQuery(con, q)