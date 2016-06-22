# use the available information and fill it up
bench2 <- function(days_) t(apply(days_, 1, fill_vector))