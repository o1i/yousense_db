# This script produces activity vectors from the database. per user.
rm(list=ls())


# ------------------------------------------------------------------------------
# --- Specific functions -------------------------------------------------------
# ------------------------------------------------------------------------------
# Plot some masts
plot_masts <- function(mastnames_, cols_, noise_ = 0){
  # requires a data.frame with "seen masts" with 3301-projected coordinates
  mastnames_ <- mastnames_[!is.na(mastnames_)]
  p <- SpatialPoints(as.data.frame(seen_masts[mastnames_, c("mast_x", "mast_y")]
                                   + matrix(rnorm(2*length(mastnames_), 
                                                  sd = noise_), ncol = 2)),
                     CRS("+init=epsg:3301")) %>%spTransform(CRS("+init=epsg:4326"))
  leaflet() %>% addTiles() %>% addCircleMarkers(data = p,
                                                color = cols_)
}
plot_masts(unique(days_used), cols, 0)





# ------------------------------------------------------------------------------
# --- Get the daily data -------------------------------------------------------
# ------------------------------------------------------------------------------


user <- 174

# CDR: soure = all_cdr + masts


# --- Overview Viz -------------------------------------------------------------
# # all calls color-coded on map
# cols <- colorRampPalette(c("black", "yellow", "green", "blue", "red", "pink"))(nt)
# allcalls <- SpatialPoints(seen_masts[days[!is.na(days)], c("mast_x", "mast_y")] +
#                             rnorm(sum(!is.na(days)) * 2, sd = noise), CRS("+init=epsg:3301")) %>% 
#   spTransform(CRS("+init=epsg:4326"))
# leaflet() %>% addTiles() %>% addCircleMarkers(data = allcalls, 
#                                               color = cols[which(!is.na(days), 
#                                                             arr.ind = T)])
# 
# t <- table(days[!is.na(days)])
# t <- t[t>50]
# plot_masts(names(t), "blue")
# 
# i <- 50
# i <- i+1; plot_masts(days[i, ], cols[which(!is.na(days[i, ]))], noise_ = 20)
# 
# round(distmat[73:80, 73:80], 2)


# ------------------------------------------------------------------------------
# --- Clustering  --------------------------------------------------------------
# ------------------------------------------------------------------------------

# --- Attempt 4: as 3, but with only one connection per event ------------------



# ------------------------------------------------------------------------------
# --- Standard Days ------------------------------------------------------------
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# --- Visualise ----------------------------------------------------------------
# ------------------------------------------------------------------------------
library(RColorBrewer)

cluster <- 3
noise <- 10

cols <- colorRampPalette(c("black", "yellow", "green", "blue", "orange", "red"))(nt)
v_days <- days[temp2 == cluster, ]
v_protoday <- prototype_days[[as.character(cluster)]] + 
  rnorm(2 * nt, sd = noise)
rownames(v_protoday) <- 1:nt

v_proto_line <- make_spatial_lines(as.data.frame(v_protoday) , 
                                 crs = CRS("+init=epsg:3301")) %>%
  spTransform(CRS("+init=epsg:4326"))
v_protoday <- SpatialPoints(v_protoday, CRS("+init=epsg:3301")) %>% 
  spTransform(CRS("+init=epsg:4326"))

v_points <- SpatialPoints(seen_masts[v_days[!is.na(v_days)], c("mast_x", "mast_y")] +
  rnorm(sum(!is.na(v_days)) * 2, sd = noise), CRS("+init=epsg:3301")) %>% 
  spTransform(CRS("+init=epsg:4326"))
v_cols <- cols[which(!is.na(v_days), arr.ind = T)[, 2]]
leaflet() %>% addTiles() %>% 
  addCircleMarkers(data = v_points, color = v_cols) %>%
  addCircleMarkers(data = v_protoday, color = cols) %>%
  addPolylines(data = v_proto_line)

compare_days <- function(d1, d2, days__, noise, cols, seen_masts_){
  m1 <- !is.na(days__[d1, ])
  m2 <- !is.na(days__[d2, ])
  v1 <- seen_masts_[days__[d1, ][m1], c("mast_x", "mast_y")] +
    rnorm(sum(m1) * 2, sd = noise)
  rownames(v1) <- NULL
  v2 <- seen_masts_[days__[d2, ][m2], c("mast_x", "mast_y")] +
    rnorm(sum(m2) * 2, sd = noise)
  rownames(v2) <- NULL

  d1p <- SpatialPoints(v1, CRS("+init=epsg:3301")) %>% 
    spTransform(CRS("+init=epsg:4326"))
  d2p <- SpatialPoints(v2, CRS("+init=epsg:3301")) %>% 
    spTransform(CRS("+init=epsg:4326"))
  d1l <- make_spatial_lines(v1,  CRS("+init=epsg:3301")) %>%
    spTransform(CRS("+init=epsg:4326"))
  d2l <- make_spatial_lines(v2,  CRS("+init=epsg:3301")) %>%
    spTransform(CRS("+init=epsg:4326"))
  leaflet() %>% addTiles() %>% 
    addCircleMarkers(data = d1p, color = cols[m1], fillColor = "brown",
                     popup = as.character(days__[d1, ][m1])) %>%
    addCircleMarkers(data = d2p, color = cols[m2], fillColor = "black",
                     popup = as.character(days__[d2, ][m2])) %>%
    addPolylines(data = d1l, color = "brown") %>%
    addPolylines(data = d2l, color = "black")
}
a <- 138
a <- a + 1; compare_days(d1 = a, d2 = a, days__ = days_used, noise = 20, cols, seen_masts_)
days_used[c("106", "120"), ]
temp <- do.call(rbind, prototype_days)
temp[temp == "0"] <- "3"
compare_days(3, 3, days__ = temp, noise = 20, cols, seen_masts_)

# ------------------------------------------------------------------------------
# --- GPS  ---------------------------------------------------------------------
# ------------------------------------------------------------------------------
# GPS: source GPS_GSM
q <- paste0("SELECT * FROM segments WHERE uid = ", user, " ORDER BY t_start;")
daten <- dbGetQuery(con, q)
display_segments(subset(daten, day == 44), cols = cols)

# ------------------------------------------------------------------------------
# --- Benchmark Prediction  ----------------------------------------------------
# ------------------------------------------------------------------------------


plot_day <- function(day_, seen_masts_, m = NULL, label = "", noise = 20,
                     linecol = "blue"){
  if(is.null(m)) m <- leaflet()%>%addTiles()
  m1 <- !is.na(day_)
  v1 <- seen_masts_[day_[m1], c("mast_x", "mast_y")] +
    rnorm(sum(m1) * 2, sd = noise)
  rownames(v1) <- NULL
  d1p <- SpatialPoints(v1, CRS("+init=epsg:3301")) %>% 
    spTransform(CRS("+init=epsg:4326"))
  d1l <- make_spatial_lines(v1,  CRS("+init=epsg:3301")) %>%
    spTransform(CRS("+init=epsg:4326"))
    addCircleMarkers(m, data = d1p, color = cols[m1], fillColor = "brown",
                     popup = paste(label, "/", as.character(day_[m1]))) %>%
    addPolylines(data = d1l, color = linecol)
}


# ------------------------------------------------------------------------------
# --- Evaluation  --------------------------------------------------------------
# ------------------------------------------------------------------------------


e1 <- ecdf(eval(gt_by_day, bench1, "seen_masts"))
e2 <- ecdf(eval(gt_by_day, bench2, "seen_masts"))
e3 <- ecdf(eval(gt_by_day, better1, "seen_masts"))
x <- 10^seq(2, 4, l = 100)
plot(x, e1(x), col = 1, type = "l", log = "x")
lines(x, e2(x), col = 2, type = "l")
lines(x, e3(x), col = 3, type = "l")




