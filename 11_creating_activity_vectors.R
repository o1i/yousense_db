# This script produces activity vectors from the database. per user.

# --- Set Parameters -----------------------------------------------------------
nt <- 24  # Number of parts to split a day into.
library(reshape2)
library(plyr)
source("00_defs_funcs.R")

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

# Distance function between masts
mast_dist <- function(m1, m2, probs = T, mult_ = 300, avg_method_ = mean){
  if(any(is.na(c(m1, m2)))) return( 0.3)
  # Measures "probabilities" of both masts connected at the same gps-location
  #   or distances if at least one mast is given.
  avgs <- c(NA, NA)
  d1 <- if(mode(m1) == "character"){
    seen_masts[m1, c("mast_x", "mast_y")]
    avgs[1] <- seen_masts[m1, "neighbour_avg_dist"]
  }else{
    m1
  }
  d2 <- if(mode(m2) == "character"){
    seen_masts[m1, c("mast_x", "mast_y")]
    avgs[2] <- seen_masts[m2, "neighbour_avg_dist"]
  }else{
    m2
  }
  d <- sqrt(sum((d1 - d2)^2))
  if(probs & !all(is.na(avgs))){
    avgs[is.na(avgs)] <- avgs[!is.na(avgs)]
    avg_method_(c(sigmoid((d- (2.6289316954 * avgs[1] - 
                                0.0002054995 * avgs[1]^2))/mult_),
                 sigmoid((d- (2.6289316954 * avgs[2] - 
                                0.0002054995 * avgs[2]^2))/mult_)))
  }else{
    d
  }
}

library(plyr)
get_standard_days <- function(days_, clusters_, coords = T){
  # takes a bunch of clustered days and returns a list of representative days
  if(length(clusters_) != nrow(days_)) {
    stop("length of cluster assignments must be equal to number of days")
  }
  dl <- lapply(as.list(unique(clusters_)), 
               function(cl_) days_[clusters_ == cl_,])
  names(dl) <- unique(clusters_)
  lapply(dl, function(arr){
    unlist(apply(arr, 2, function(v){
      if(all(is.na(v))) return(t(c(NA,NA)))
      if(coords){
        return(fill_vector(colMeans(seen_masts[v[!is.na(v)], c("mast_x", "mast_y")])))
      }else{
        return(fill_vector(Mode(v)))
      }
    }))
  })
}

# ------------------------------------------------------------------------------
# --- Get the daily data -------------------------------------------------------
# ------------------------------------------------------------------------------
connect(user = "burkhard", 
        pw = scan("nosafe.txt", what = "character"), 
        dbname = "burkhard",
        host = "localhost", 
        port = 5432)

user <- 204

# CDR: soure = all_cdr + masts
q <- paste0("
SELECT a.uid, a.id_masts, 
  extract(year  from a.t_start at time zone 'Europe/Tallinn' - interval '3 hours') as y_corr,
  extract(month from a.t_start at time zone 'Europe/Tallinn' - interval '3 hours') as m_corr,
  extract(day   from a.t_start at time zone 'Europe/Tallinn' - interval '3 hours') as d_corr,
  extract(epoch from AGE(a.t_start at time zone 'Europe/Tallinn' - interval '3 hours', 
     (a.t_start at time zone 'Europe/Tallinn' - interval '3 hours')::date)) / 3600/24 as frac,
  st_x(st_transform(b.geom, 3301)) as mast_x, 
  st_y(st_transform(b.geom, 3301)) as mast_y,
  b.neighbour_avg_dist
FROM all_cdr a LEFT JOIN masts b
  ON a.id_masts = b.id_masts
WHERE a.uid = ", user, " AND
  a.id_masts is not NULL
;")

# GSM: source = gps_gsm
q2 <- paste0("
SELECT a.uid, a.id_masts, 
  extract(year  from a.t_start at time zone 'Europe/Tallinn' - interval '3 hours') as y_corr,
  extract(month from a.t_start at time zone 'Europe/Tallinn' - interval '3 hours') as m_corr,
  extract(day   from a.t_start at time zone 'Europe/Tallinn' - interval '3 hours') as d_corr,
  extract(epoch from AGE(a.t_start at time zone 'Europe/Tallinn' - interval '3 hours', 
     (a.t_start at time zone 'Europe/Tallinn' - interval '3 hours')::date)) / 3600/24 as frac,
  st_x(st_transform(a.geom_mast, 3301)) as mast_x, 
  st_y(st_transform(a.geom_mast, 3301)) as mast_y,
  a.neighbour_avg_dist
FROM gps_gsm as a
WHERE a.uid = ", user, " AND
  a.geom_mast is not NULL
;")
system.time(t <- dbGetQuery(con,q2))

masts_sp <- SpatialPoints(t[, c("mast_x", "mast_y")], CRS("+init=epsg:3301")) %>%
  spTransform(CRS("+init=epsg:4326"))
leaflet() %>% addTiles() %>% addCircleMarkers(data = masts_sp, 
                                              popup = as.character(t$id_masts))

seen_masts <- unique(t[, c("id_masts", "mast_x", "mast_y", 
                           "neighbour_avg_dist")])
rownames(seen_masts) <- seen_masts$id_masts

t$rest <- (t$frac %% (1/nt))*nt
t$frac <- t$frac %/% (1/nt)
t$id_masts <- as.character(t$id_masts)
t2 <- ddply(t, c("uid", "y_corr", "m_corr", "d_corr", "frac"), 
           .fun = function(df){
             df[which.min(abs(df$rest - 0.5)),]
           })
t2$temp <- as.numeric(t2$id_masts)
t2 <- rbind(data.frame(uid = 0, id_masts = 0, y_corr = 0, m_corr = 0, 
                       d_corr = 0, frac = 1:nt - 1, mast_x = 0, mast_y = 0,
                       neighbour_avg_dist = 0, rest = 0, temp = 0), t2)
days <- acast(t2[, c("uid", "y_corr", "m_corr", "d_corr", 
                                       "frac", "id_masts")], 
              uid + y_corr + m_corr + d_corr ~ frac, 
      fill = "", 
      value.var = "id_masts",
      fun.aggregate = paste0)[-1, ]
days[days == ""] <- NA
colnames(days) <- (as.numeric(colnames(days)) + 3) %% 24
rownames(days) <- gsub("_", "-",gsub("^[^_]*_", "", rownames(days))) %>%
  as.Date() %>% strftime(format = "%j") %>% as.numeric()
may <- substr(rownames(days), 1, 10) == "174_2015_5"

# --- Overview Viz -------------------------------------------------------------
# all calls color-coded on map
cols <- colorRampPalette(c("black", "yellow", "green", "blue", "red", "pink"))(nt)
allcalls <- SpatialPoints(seen_masts[days[!is.na(days)], c("mast_x", "mast_y")] +
                            rnorm(sum(!is.na(days)) * 2, sd = noise), CRS("+init=epsg:3301")) %>% 
  spTransform(CRS("+init=epsg:4326"))
leaflet() %>% addTiles() %>% addCircleMarkers(data = allcalls, 
                                              color = cols[which(!is.na(days), 
                                                            arr.ind = T)])

t <- table(days[!is.na(days)])
t <- t[t>50]
plot_masts(names(t), "blue")

i <- 50
i <- i+1; plot_masts(days[i, ], cols[which(!is.na(days[i, ]))], noise_ = 20)

round(distmat[73:80, 73:80], 2)


# ------------------------------------------------------------------------------
# --- Clustering  --------------------------------------------------------------
# ------------------------------------------------------------------------------

# --- Attempt 1: attraction and repulsion. Connection to NA is in the middle ---
value_mat <- 2^-abs(1 * outer(1:nt, 1:nt, "-"))
dist_offset <- sum(-value_mat)

dist_fun_1 <- function(v1, v2, value_mat_, offset_){
  # Simple distance function: weights are given by temporal exp. decay
  # Offset is the minimal achievable distance
  # -1, NA, +1 is equavalent to 0, 1, 2 but with less calculations (many NA)
  return(sum(value_mat_ * (-2 * abs(outer(v1, v2, "==")) + 1), 
              na.rm = T) - offset_)
}

distmat <- matrix(NA, nrow = nrow(days), ncol = nrow(days))
for(i in 2:nrow(days)){
  for(j in 1:(i - 1)){
    distmat[i, j] <- dist_fun_1(days[i, ], days[j, ], value_mat, dist_offset)
  }
}
image(distmat[may, may])
dists <- as.dist(distmat)
temp <- hclust(dists, "ward.D2")
temp2 <- cutree(temp, k = 3)
# plot(jitter(temp2), jitter(apply(days, 1, function(v)sum(!is.na(v)))))
# days[c(which(temp2 == 1), which(temp2 == 2), which(temp2 == 3)),] <- days
# --> Result: does not really work, as the offset of the distances is too large.

# --- Attempt 2: sigmoid attraction with just close neighbours -----------------
value_mat_2 <- abs(1 * outer(1:nt, 1:nt, "-"))
value_mat_2 <- (value_mat_2 < 2) + (0.5* (value_mat_2 == 2))
dist_fun_2 <- function(v1, v2, value_mat_, offset_){
  # Simple distance function: weights are given by temporal exp. decay
  # Offset is the minimal achievable distance
  # -1, NA, +1 is equavalent to 0, 1, 2 but with less calculations (many NA)
  return(sigmoid(0.2 * sum(value_mat_ * (-2 * abs(outer(v1, v2, "==")) + 1), 
             na.rm = T) - offset_))
}
distmat <- matrix(NA, nrow = nrow(days), ncol = nrow(days))
for(i in 2:nrow(days)){
  for(j in 1:(i - 1)){
    distmat[i, j] <- dist_fun_2(days[i, ], days[j, ], value_mat, 0)
  }
}
image(distmat[may, may])
dists <- as.dist(distmat)
temp <- hclust(dists, "ward.D2")
temp2 <- cutree(temp, k = 3)

# --- Attempt 3: sigmoid clustering with distances -----------------------------
# value_mat_3 <- matrix(epa(abs(1 * outer(1:nt, 1:nt, "-"))/4), nt)
value_mat_3 <- value_mat_2

dist_fun_3 <- function(d1, d2, value_mat_, mult_, method_){
  mask <- outer(d1, d2, FUN = function(v1, v2) !is.na(v1) & !is.na(v2)) * 
    value_mat_
  if(sum(mask) == 0){
    return(0.5)
  }else{
    filled <- which(mask>0, arr.ind = T)
    scores <- apply(filled, 1, FUN = function(v){
      mast_dist(d1[v[1]], d2[v[2]], mult_, method_)
    })
    # Penalise cases with few but matching masts
    alpha <- min(1, (length(scores) + 20) / 40)
    return(alpha * sum(scores * value_mat_[filled] + (1-scores) * 0.5) / 
             sum(value_mat_[filled]) + (1-alpha) * 0.5)
  }
}
distmat <- matrix(NA, nrow = nrow(days), ncol = nrow(days))
for(i in 2:nrow(days)){
  for(j in 1:(i - 1)){
    distmat[i, j] <- dist_fun_3(days[i, ], days[j, ], value_mat_3, 300, mean)
  }
}
image(distmat)
dists <- as.dist(distmat)
temp <- hclust(dists, "ward.D2")
temp2 <- cutree(temp, k = 3)

# --- Attempt 4: as 3, but with only one connection per event ------------------
library(parallel)
value_mat_4 <-value_mat_2
get_relevant_value <- function(v) if(min(v)<0.5){min(v)}else{max(v)}
dist_fun_4 <- function(d1, d2, value_mat_, mult_, method_){
  mask <- outer(d1, d2, FUN = function(v1, v2) !is.na(v1) & !is.na(v2)) * 
    value_mat_
  if(sum(mask) == 0){
    return(0.5)
  }else{
    filled <- which(mask>0, arr.ind = T)
    scores <- apply(filled, 1, FUN = function(v){
      mast_dist(d1[v[1]], d2[v[2]], probs = T, mult_, method_)
    })
    score_1 <- mask[filled] * scores + (1-mask[filled]) * 0.5
    score_2 <- c(aggregate(score_1, by = list(filled[, 1]), 
                           FUN = get_relevant_value)$x,
                 aggregate(score_1, by = list(filled[, 2]), 
                           FUN = get_relevant_value)$x,
                 rep(0.5, 2 * nt - length(unique(filled[, 1])) - 
                       length(unique(filled[, 2]))))
    w <- 1 - (0.95 * (score_2 == 0.5))
    return(sum(score_2 * w) / sum(w))
  }
}
distmat <- matrix(NA, nrow = nrow(days), ncol = nrow(days))
n <- nrow(days)
ind <- 1:n^2
ind <- ind[((ind - 1) %% n) + 1 > ((ind - 1) %/% n + 1)]
distmat[ind] <- unlist(mclapply(ind, FUN = function(i_) {
  dist_fun_4(d1 = days[((i_ - 1) %% n) + 1, ], 
             d2 = days[((i_ - 1) %/% n + 1), ], 
             value_mat_ = value_mat_4, 
             mult_ = 300, 
             method_ = mean)
}, mc.cores = 4))


diag(distmat) <- apply(days, 1, FUN = function(v) sum(!is.na(v))/nt)
image(distmat)
dists <- as.dist(distmat)
temp <- hclust(dists, "ward.D2")
temp2 <- cutree(temp, k = 3)

# ------------------------------------------------------------------------------
# --- Standard Days ------------------------------------------------------------
# ------------------------------------------------------------------------------
prototype_days <- get_standard_days(days, temp2, coords = F)

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

compare_days <- function(d1, d2, noise, cols){
  m1 <- !is.na(days[d1, ])
  m2 <- !is.na(days[d2, ])
  v1 <- seen_masts[days[d1, ][m1], c("mast_x", "mast_y")] +
    rnorm(sum(m1) * 2, sd = noise)
  rownames(v1) <- NULL
  v2 <- seen_masts[days[d2, ][m2], c("mast_x", "mast_y")] +
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
    addCircleMarkers(data = d1p, color = cols[m1], fillColor = "brown") %>%
    addCircleMarkers(data = d2p, color = cols[m2], fillColor = "black") %>%
    addPolylines(data = d1l, color = "brown") %>%
    addPolylines(data = d2l, color = "black")
}
compare_days(287, 287, 20, cols)


# ------------------------------------------------------------------------------
# --- GPS  ---------------------------------------------------------------------
# ------------------------------------------------------------------------------
# GPS: source GPS_GSM
q <- paste0("SELECT * FROM segments WHERE uid = ", user, " ORDER BY t_start;")
daten <- dbGetQuery(con, q)
display_segments(subset(daten, day == 325), cols = cols)

# ------------------------------------------------------------------------------
# --- Benchmark Prediction  ----------------------------------------------------
# ------------------------------------------------------------------------------
bench1 <- matrix(rep(apply(days, 2, Mode), each = nrow(days)), 
                 nrow = nrow(days))
rownames(bench1) <- rownames(days)
bench2 <- t(apply(days, 1, fill_vector))

# ------------------------------------------------------------------------------
# --- Better Predictions  ------------------------------------------------------
# ------------------------------------------------------------------------------
better1 <- t(apply(days, 1, function(d_){
  # assumes both d and the prototype days to be masts
  propose <- prototype_days[[which.min(sapply(prototype_days, function(ptd){
    daywarp(d1 = d_, d2 = ptd, d = mast_dist, w = 2, probs = T)
  })
  )]]
  propose[!is.na(d_)] <- d_[!is.na(d_)]
  return(propose)
}))
rownames(better1) <- rownames(days)

# ------------------------------------------------------------------------------
# --- Ground Truth  ------------------------------------------------------------
# ------------------------------------------------------------------------------
hour_shift <- 3
# --- 1: Evaluate against stop segments ----------------------------------------
library(lubridate)
q <- paste0("
SELECT uid, t_start, t_end, st_x(st_transform(geom_mean, 3301)) AS x_mean, 
  st_y(st_transform(geom_mean, 3301)) AS y_mean, 
  extract(DOY FROM t_start - INTERVAL '", hour_shift, " hours')::smallint as day,
  extract(DOY FROM t_end - INTERVAL '", hour_shift, " hours')::smallint AS  day_next,
  valid_start, valid_end, stop
FROM 
  segments
WHERE 
  uid = ", user, "
;")
system.time(ground_truth <- dbGetQuery(con,q))

ground_truth$f_start <- day_frac(ground_truth$t_start - 
                                   as.duration(hour_shift * 3600))
ground_truth$f_end   <- day_frac(ground_truth$t_end   - 
                                   as.duration(hour_shift * 3600))

gt_by_day <- sapply(min(ground_truth$day):max(ground_truth$day_next), 
                    simplify = F,
                    FUN = function(day_){
                      subset(ground_truth, day <= day_ & day_next >= day_)
                    })
names(gt_by_day) <- min(ground_truth$day):max(ground_truth$day_next)
gt_by_day <- gt_by_day[which(sapply(gt_by_day, nrow) != 0)]
for(i in 1:length(gt_by_day)){
  n <- nrow(gt_by_day[[i]])
  if(gt_by_day[[i]]$day[1] != gt_by_day[[i]]$day_next[1]){
    gt_by_day[[i]][1, "f_start"] <- 0
  } 
  if(gt_by_day[[i]]$day[n] != gt_by_day[[i]]$day_next[n]){
    gt_by_day[[i]][n, "f_end"] <- 1
  } 
}

# ------------------------------------------------------------------------------
# --- Evaluation  --------------------------------------------------------------
# ------------------------------------------------------------------------------

day_comp_1 <- function(gt_, day_, masts_){
  # Computes the average distance between ground truth and prediction
  # compares a ground truth segments data frame (containing x_mean, y_mean,
  #   f_start, f_end) anda predicted 
  # day_, which takes the form of a prediction at regular intervals (either
  #   as strings, in which cases the masts table is needed or in coordinates xy
  #   named mast_x and mast_y)
  # masts is only a string of the name of the actual masts
  if(mode(day_) == "numeric"){
    if(ncol(day_) > nrow(day_)) day_ <- t(day_)
    breaks <- seq(0, 1, l = nrow(day_) + 1)
  }else{
    breaks <- seq(0, 1, l = length(day_) + 1)
    day_ <- get(masts_)[day_, c("mast_x", "mast_y")]
  }
  res <- apply(as.matrix(gt_[, c("x_mean", "y_mean", 
                                 "f_start", "f_end")]), 1,
               FUN = function(v_){
                 inds <- (which.min(breaks <= v_[3])-1):
                   (which.max(breaks >= v_[4]) - 1)
                 comp <- day_[inds, , drop = F]
                 dists <- apply(as.matrix(comp - rep(v_[1:2], 
                                                     each = nrow(comp)), 
                                          ncol = 2), 
                                1, function(w_) sqrt(sum(w_^2)))
                 weights <- pmin(v_[4], breaks[pmin(inds + 1, length(breaks))])-
                   pmax(v_[3], breaks[inds])
                 if(length(weights) != length(dists)) {cat(v_); flush.console()}
                 c(penalty = sum(weights * dists), duration = sum(weights))
               })
  return(sum(res[1, ]) / sum(res[2, ]))
}

eval <- function(ground_truth_, prediction_, masts_){
  days_to_test <- intersect(names(ground_truth_), rownames(prediction_))
  dist_vec <- sapply(days_to_test, function(d_){
    day_comp_1(gt_ = ground_truth_[[d_]], day_ = prediction_[d_, ], 
               masts_ = masts_)
  })
  return(dist_vec)
}
a <- eval(gt_by_day, bench1, "seen_masts")
b <- eval(gt_by_day, bench2, "seen_masts")
c <- eval(gt_by_day, better1, "seen_masts")

