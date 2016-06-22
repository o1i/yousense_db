bench1 <- function(days_) {
  # Irrespective of what happened, return the most common mast per time.
  a <- matrix(rep(fill_vector(apply(days_, 2, Mode)), 
                                     each = nrow(days_)), 
                                 nrow = nrow(days_))
  rownames(a) <- rownames(days)
  return(a)
}