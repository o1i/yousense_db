# Analysis of the relationship between the handover-location and the GPS

# --- Front matters ------------------------------------------------------------
library("RPostgreSQL")
try(disconnect(), silent = T)
connect(user = "burkhard", 
        pw = scan("nosafe.txt", what = "character"), 
        dbname = "burkhard",
        host = "localhost", 
        port = 5432)

# --- Actual analysis ----------------------------------------------------------
# I would like to find out:
# 1. Distribution of the distance to the last connected antenna, taking into 
#    account the different sizes of voronoi Polygons.
# 2. At the time of a handover: What radio type and what antenna is chosen?
#    i.e. Is it always the "highest possible" if visible? and
#    what is the ratio of the actual distance to the radio of the chosen type
#    to the shortest possible distance to an antenna of the same type?
# 3. At the point of the handover: is the antenna closer than on average?

# For this I need:For every GPS record: 
# a. radio type and location of the connected & the closest mast of all types
#    and networks
# b. connection mode at the time plus network

q <- "
create or replace view gps_enriched as
select a.gps_id, a.uid, a.t, a.accuracy, a.geom as geom_gps, x.state, x.operator_short, x.t_state,
y.t_connect, y.geom_connected, y.net_connected, y.radio as radio_connected
from gps a left join lateral (
  select state, operator_short, t as t_state
  from device_cell_service
  where a.uid = uid and a.t > t
  order by t desc
  limit 1
) x
on true
left join lateral(
  select c.t as t_connect, c.geom_connected, c.net_connected, c.radio
  from (
    select d.uid, d.t, e.geom as geom_connected, d.net as net_connected,
      e.radio
    from device_cell_location d left join masts e
    on d.mcc = e.mcc and d.net = e.net and d.area = e.area and d.cell = e.cell
  ) c
  where a.uid = c.uid and a.t > c.t 
  order by c.t desc
  limit 1
) y
on true
;
"
dbGetQuery(con, q)

# --- Closest mast info --------------------------------------------------------
# Achtung, dauert lange! Zu lange! abgeschossen.
# q <- "
# DROP TABLE IF EXISTS closest_masts CASCADE;
# create table closest_masts as
# select a.gps_id, a.net_connected, a.radio_connected, b.net as net_mast, b.radio, 
#   b.geom as geom_mast, ST_Distance(ST_Transform(a.geom_gps, 3301), 
#                                    ST_Transform(b.geom, 3301)) as dist
# from
# gps_enriched a left join masts b
# ON ST_Contains(ST_Transform(b.geom_voronoi, 3301), 
#                ST_Transform(a.geom_gps, 3301)) and 
#   b.mcc = 248
# ;
# "
# dbGetQuery(con, q)

