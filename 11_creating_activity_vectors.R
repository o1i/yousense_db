# This script produces activity vectors from the database. per user.

# --- Set Parameters -----------------------------------------------------------
nt <- 24  # Number of parts to split a day into.
library(reshape2)
library(plyr)
source("00_defs_funcs.R")

# ------------------------------------------------------------------------------
# --- Get the daily data -------------------------------------------------------
# ------------------------------------------------------------------------------
connect(user = "burkhard", 
        pw = scan("nosafe.txt", what = "character"), 
        dbname = "burkhard",
        host = "localhost", 
        port = 5432)

q <- "
SELECT a.uid, a.id_masts, 
  extract(year  from a.t_start - interval '3 hours') as y_corr,
  extract(month from a.t_start - interval '3 hours') as m_corr,
  extract(day   from a.t_start - interval '3 hours') as d_corr,
  extract(epoch from AGE(a.t_start - interval '3 hours', 
     (a.t_start - interval '3 hours')::date)) / 3600/24 as frac,
  st_x(st_transform(b.geom, 3301)) as mast_x, 
  st_y(st_transform(b.geom, 3301)) as mast_y,
  b.neighbour_avg_dist
FROM all_cdr a LEFT JOIN masts b
  ON a.id_masts = b.id_masts
WHERE a.uid = 174 AND
  a.id_masts is not NULL
;"
system.time(t <- dbGetQuery(con,q))

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
may <- substr(rownames(days), 1, 10) == "174_2015_5"

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
plot(jitter(temp2), jitter(apply(days, 1, function(v)sum(!is.na(v)))))
days <- days[c(which(temp2 == 1), which(temp2 == 2), which(temp2 == 3)),]
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
plot(jitter(temp2), jitter(apply(days, 1, function(v)sum(!is.na(v)))))
days <- days[c(which(temp2 == 1), which(temp2 == 2), which(temp2 == 3)),]

# --- Attempt 3: sigmoid clustering with distances -----------------------------
value_mat_3 <- matrix(epa(abs(1 * outer(1:nt, 1:nt, "-"))/4), nt)
mast_dist <- function(m1, m2, mult, avg_method){
  # Measures "probabilities" of both masts connected at the same gps-location
  d <- sqrt(sum((seen_masts[as.character(m1), c("mast_x", "mast_y")] - 
    seen_masts[as.character(m2), c("mast_x", "mast_y")])^2))
  avgs <- seen_masts[as.character(c(m1, m2)), "neighbour_avg_dist"]
  avg_method(c(sigmoid((d- (2.6289316954 * avgs[1] - 
                                         0.0002054995 * avgs[1]^2))/mult),
                          sigmoid((d- (2.6289316954 * avgs[2] - 
                                         0.0002054995 * avgs[2]^2))/mult)))
}
dist_fun_3 <- function(d1, d2, value_mat_, mult_, method_){
  mask <- outer(d1, d2, FUN = function(v1, v2) !is.na(v1) & !is.na(v2)) * 
    value_mat_
  if(sum(mask) == 0){
    return(0.5)
  }else{
    relevant <- which(mask>0, arr.ind = T)
    scores <- apply(relevant, 1, FUN = function(v){
      mast_dist(d1[v[1]], d2[v[2]], mult_, method_)
    })
    # Penalise cases with few but matching masts
    alpha <- min(1, (length(scores) + 20) / 40)
    return(alpha * sum(scores * value_mat_[relevant]) / 
             sum(value_mat_[relevant]) + (1-alpha) * 0.5)
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
plot(jitter(temp2), jitter(apply(days, 1, function(v)sum(!is.na(v)))))
days <- days[c(which(temp2 == 1), which(temp2 == 2), which(temp2 == 3)),]






