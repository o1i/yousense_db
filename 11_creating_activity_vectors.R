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
SELECT uid, masts_id, 
  extract(year  from t_start - interval '3 hours') as y_corr,
  extract(month from t_start - interval '3 hours') as m_corr,
  extract(day   from t_start - interval '3 hours') as d_corr,
  extract(epoch from AGE(t_start - interval '3 hours', 
     (t_start - interval '3 hours')::date)) / 3600/24 as frac
FROM all_cdr
WHERE uid = 174 AND
  masts_id is not NULL
;"
t <- dbGetQuery(con,q)
t$rest <- (t$frac %% (1/nt))*nt
t$frac <- t$frac %/% (1/nt)
t$masts_id <- as.character(t$masts_id)
t2 <- ddply(t, c("uid", "y_corr", "m_corr", "d_corr", "frac"), 
           .fun = function(df){
             df[which.min(abs(df$rest - 0.5)),]
           })
t2$temp <- as.numeric(t2$masts_id)
t2 <- rbind(data.frame(uid = 0, masts_id = 0, y_corr = 0, m_corr = 0, 
                       d_corr = 0, frac = 1:nt - 1, rest = 0, temp = 0), t2)
days <- acast(t2[, c("uid", "y_corr", "m_corr", "d_corr", 
                                       "frac", "masts_id")], 
              uid + y_corr + m_corr + d_corr ~ frac, 
      fill = "", 
      value.var = "masts_id",
      fun.aggregate = paste0)[-1, ]
days[days == ""] <- NA
may <- substr(rownames(days), 1, 10) == "174_2015_5"

# ------------------------------------------------------------------------------
# --- Definde distances --------------------------------------------------------
# ------------------------------------------------------------------------------
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
