get_empty_user_char <- function(){
  names <- c("uid",
             "avg_entropy_within_hour",
             "med_entropy_within_hour",
             "sd_entropy_within_hour",
             "entropy_accross_hours",
             "avg_cdr_per_day",
             "med_cdr_per_day",
             "sd_cdr_per_day",
             "avg_time_spanned",
             "med_time_spanned",
             "sd_time_spanned",
             "avg_distance_travelled",
             "med_distance_travelled",
             "sd_distance_travelled",
             "num_days",
             "num_days_3cdr",
             "num_days_4cdr",
             "num_days_5cdr")
  mat <- matrix(NA, ncol = length(names), nrow = 0)
  colnames(mat) <- names
  as.data.frame(mat)
}

user_char <- function(days_, gt_, user_){
  library(entropy)
  entropy_within_hour <- apply(days_, 2, function(v) entropy(table(v[!is.na(v)])))
  cdr_per_day <- apply(days_, 1, function(v)sum(!is.na(v)))
  time_spanned <- apply(days_, 1, function(v) max(which(!is.na(v)))-min(which(!is.na(v))))
  distance_travelled <- sapply(gt_, function(df_) sum(sqrt(diff(df_[df_$stop, "x_mean"])^2 + diff(df_[df_$stop, "y_mean"])^2)))
  daily_cdr <- apply(as.matrix(days_), 1, function(v) sum(!is.na(v)))
  
  out_df <- data.frame(
    uid = user_,
    avg_entropy_within_hour = mean(entropy_within_hour),
    med_entropy_within_hour = median(entropy_within_hour),
    sd_entropy_within_hour = sd(entropy_within_hour),
    entropy_accross_hours = entropy(apply(days_, 2, function(v) sum(!is.na(v)))),
    avg_cdr_per_day = mean(cdr_per_day),
    med_cdr_per_day = median(cdr_per_day),
    sd_cdr_per_day = sd(cdr_per_day),
    avg_time_spanned = mean(time_spanned),
    med_time_spanned = median(time_spanned),
    sd_time_spanned = sd(time_spanned),
    avg_distance_travelled = mean(distance_travelled),
    med_distance_travelled = median(distance_travelled),
    sd_distance_travelled = sd(distance_travelled),
    num_days = nrow(days_),
    num_days_3cdr = sum(daily_cdr >=3),
    num_days_4cdr = sum(daily_cdr >=4),
    num_days_5cdr = sum(daily_cdr >=5)
  )
  return(out_df)
}