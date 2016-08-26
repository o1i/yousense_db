# The functions in this Script help create example user days for evaluation
# based on the masts around Tartu.


y_ <- 58.3780871
x_ <- 26.72343
srid_ <- 4326
buffer_ <- 20000
get_masts_of_area <- function(x_, y_, buffer_, srid_ = 4326){
  # gets the masts around a specific point
  
  # input: 
  #         x_, y_      coordinates of the centre point
  #         buffer_     buffer in meters around the point (in m)
  #         srid_       CRS of the input (will be transformed to 3301)
  
  # output:
  #         a dataframe with the mast ids as well as their coordinates and
  #         distances from the centre point.
  
  q <- paste0("select id_masts, ST_X(ST_Transform(geom, 3301)) as mast_x, 
ST_Y(ST_Transform(geom, 3301)) as mast_y,
neighbour_avg_dist,
id_masts as cluster,
ST_X(geom) as lon,
ST_Y(geom) as lat,
ST_Distance(ST_Transform(geom, 3301), 
            ST_Transform(ST_pointFromText('POINT(", x_, " ",  y_, ")', ", 
                                          srid_, "),
                         3301)) as dist
FROM masts a WHERE
ST_CONTAINS(
  ST_Buffer(
    ST_Transform(
      ST_PointFromText(
        'POINT(", x_, " ",  y_, ")'
      , ", srid_, "
    )
    , 3301
  )
  , ", buffer_, "
)
  ,  ST_Transform(a.geom, 3301))")
masts <- dbGetQuery(con,q)
rownames(masts) <- masts$id_masts
masts$id_masts <- as.character(masts$id_masts)
return(masts)
}

hour_to_nt <- function(hour_, nt_, hour_shift_ = 3){
  # calculates the index based on nt_ and hour_shift from the hour
  
  # input:
  #         hour_       hour of the day
  #         nt_         length of the vector
  #         hour_shift_ functional midnight
  
  # output:
  #         the index corresponding to the hour
  floor(((hour_ - hour_shift_) %% 24) / 24 * nt_) + 1
}

index_to_hour <- function(index_, nt_, hour_shift_){
  (hour_shift_ + (index_ - 1) / nt_ * 24) %% 24
}

create_routine <- function(masts_, times_, durations_, nt_, home_ = NA){
  # creates a typical day vector with a given number of deviances from the home
  
  # input:
  #         masts_      vector with available id_masts
  #         times_      starting indices of the deviations
  #         duration_   durations of the deviations
  #         nt_         length of the vector
  #         home_       name of the home mast
  
  # output:
  #         a vector of length nt_ with a typical day
  places <- sample(masts_, length(times_), replace = T)
  outvec <- rep(home_, nt_)
  for(i in seq(along=times_)){
    outvec[times_[i]:(times_[i] + durations_[i] - 1)] <- places[i]
  }
  return(outvec)
}

get_no_of_stops <- function(count_, mean_ = 2, min_ = 0, max_ = 4){
  # creates the number of deviations from home for a routine
  
  # input:
  #         count_      number of numbers to create
  #         mean_       average value
  
  # output:
  #         vector of length count_ with the number of non-home-stops
  pmax(min_, pmin(max_, rpois(count_, lambda = mean_)))
}

get_durations_of_stops <- function(count_, mean_){
  # Creates the durations of stops
  
  # input:
  #         count_      number of durations to create
  #         mean_       average value
  
  # output:
  #         vector of length count_ with the duration values in hours
  ceiling(runif(count_) * 2 * mean_)
}

get_starts_of_stops <- function(durations_, nt_, hour_shift_){
  # Creates the starting indices of stops
  
  # input:
  #         durations_  durations in hours
  #         nt_         length of the vector
  #         hour_shift_ the starting hour of the day
  
  
  # output:
  #         starting indices of the deviations (not hours!)
  starts <- numeric(length(durations_))
  starts <- floor(runif(length(durations_), 7, 20))
  starts[durations_ > 3] <- floor(runif(sum(durations_ > 3), 11, 18))
  starts[durations_ > 6] <- floor(runif(sum(durations_ > 6), 7, 12))
  starts <- hour_to_nt(starts, nt_ = nt_, hour_shift_ = hour_shift_)
  return(starts)
}

masts_check <- function(mast_list_, masts_, lim_){
  # checks whether the routine members have a minimum distance
  
  # input:
  #         masts_      mast_list_ dataframe with id_masts rownames and
  #                       x and y columns
  #         mast_list_  vector with proposed masts
  #         lim_        threshold on distance to be acceptable
  
  # output:
  #         boolean whether the minimal distance is large enough
  #         empty routines return inf as minimal dist and are thus accepted
  if(length(mast_list_) <= 1){
    return(T)
  }else{
    dists <- dist(masts_[as.character(mast_list_), c("mast_x", "mast_y")])
    return(min(dists[dists > 0]) >= lim_)
  }
}



create_routine_2 <- function(masts_, nt_, mean1_, mean2_, home_, 
                             hour_shift_ = 3,
                             lim_ = 1000,
                             min_ = 0, max_ = 4){
  # Creates the routines in list format
  
  # input:
  #         masts_      dataframe of available masts
  #         nt_         length of a vector
  #         mean1_      mean for number of stops
  #         mean2_      mean for durations
  #         home_       id of home. needed for the distance check
  #         lim_        minimum distance between two locations
  #         min_        minimum number of stops in a routine
  #         max_        maximum number of stops in a routine
  
  # output:
  #         list with the deviations (id_masts, start_index, duration)
  num <- get_no_of_stops(count_ = 1, mean_ = mean1_, min_ = min_, max_ = max_)
  durations <- pmin(9, get_durations_of_stops(count_ = num, mean_ = mean2_))
  starts <- get_starts_of_stops(durations_ = durations, nt_, hour_shift_)
  places <- sample(setdiff(masts_$id_masts, home_), num, replace = T)
  check <- masts_check(mast_list_ = c(places, home_), masts_ = masts_, lim_)
  ii <- 0
  while(!check & ii < 100){
    places <- sample(masts_$id_masts, num, replace = T)
    check <- masts_check(mast_list_ = c(places, home_), masts_ = masts_, lim_)
    ii <- ii + 1
  }
  return(data.frame(id_masts = places, 
                    start_index = starts, 
                    duration =durations,
                    stringsAsFactors = F))
}

create_routine_3 <- function(masts_, nt_, home_, 
                             hour_shift_ = 3,
                             lim_ = 1000){
  # Creates the routines in list format
  
  # input:
  #         masts_      dataframe of available masts
  #         nt_         length of a vector
  #         home_       id of home. needed for the distance check
  #         lim_        minimum distance between two locations
  
  # output:
  #         list with the deviations (id_masts, start_index, duration)
  num <- 3
  durations <- c(5, 5, 4)
  starts <- c(4, 9, 14)
  places <- sample(setdiff(masts_$id_masts, home_), num, replace = T)
  check <- masts_check(mast_list_ = c(places, home_), masts_ = masts_, lim_)
  ii <- 0
  while(!check & ii < 100){
    places <- sample(masts_$id_masts, num, replace = T)
    check <- masts_check(mast_list_ = c(places, home_), masts_ = masts_, lim_)
    ii <- ii + 1
  }
  return(data.frame(id_masts = places, 
                    start_index = starts, 
                    duration =durations,
                    stringsAsFactors = F))
}

shift_routine_2 <- function(routine_){
  # shifts the starting times of the routine by 0-2 hours
  
  # input:
  #         routine_    a routine
  
  # output:
  #         a routine with (potentially) shifted starting times
  n <- nrow(routine_)
  shift <- pmax(-2, pmin(2, round(rnorm(n * 2, sd = 1))))
  si <- routine_$start_index + shift[1:n]
  durations <- pmax(1, routine_$duration - shift[1:n] + shift[(n + 1):(2 * n)])
  routine_$start_index <- si
  routine_$duration <- durations
  return(routine_)
}

get_noise <- function(masts_, count_, fixed_ = T, nt_ = 24, hour_shift_ = 3){
  # get some noise masts and times
  
  # input:
  #         masts_      list of available masts
  #         count_      number of noises to produce / mean of poisson counts
  #         fixed_      if F then a random poisson count is taken
  
  # output:
  #         data frame with id_masts|hour
  if(!fixed_) count_ <- rpois(1, count_)
  masts <- sample(masts_, count_, replace = T)
  indices <- hour_to_nt(floor(runif(count_, 6, 22)), 
                        nt_ = nt_, hour_shift_ = hour_shift_)
  return(data.frame(id_masts = masts, index = indices))
}

make_cdr_gt <- function(home_, routine_, noise_, nt_ = 24){
  # create a realisation of the cdr "ground truth". Leave nt_ == 24!
  
  # input:
  #         home_       id of the home mast
  #         routine_    data frame with the routine
  #         noise_      data frame with noise
  
  # output:
  #         vector with the ground truth
  
  outvec <- rep(home_, nt_)
  if(nrow(routine_) > 0){
    for(ii in 1:nrow(routine_)){
      outvec[routine_$start_index[ii] + 0:(routine_$duration[ii] - 1)] <- 
        routine_$id_masts[ii]
    }
  }
  if(nrow(noise_) > 0){
    for(ii in 1:nrow(noise_)){
      outvec[noise_$index] <- noise_$id_masts
    }
  }
  return(outvec)
}

get_calling_probs <- function(nt_ = 24, hour_shift_ = 3, equal = 0){
  # get the vector with normalised probabilities of a call from the database
  
  # input:
  #         nt_         length of vector
  #         hour_shift_ functional midnight  
  #         equal_      if true, then all probs are 1/n
  
  # output:
  #         vector with the probabilities of a call
  e <- rep(1/nt_, nt_)
  if(equal == 1) return(e)
  q <- paste0("
    select a.index, count(a.index) as p from (
    select floor(extract(epoch from AGE(t_start at time zone 'Europe/Tallinn' - 
    interval '", hour_shift, " hours', 
               (t_start at time zone 'Europe/Tallinn' - interval '", hour_shift,
    " hours')::date)) / 3600/24*", nt_, ") + 1 as index
    from all_cdr) a
    group by a.index
    order by a.index")
  hours <- dbGetQuery(con, q)
  hours$p <- hours$p / sum(hours$p)
  return(hours * (1-equal) + e * equal)
}

get_user_day <- function(cdr_gt_, calling_probs_, scale_ = 1){
  # Create a realisation of the ground truth
  
  # input:
  #         cdr_gt_     vector with the ground truth
  #         calling_probs_ probability of a call
  #         scale_      multiplicative factor on probs
  
  # output:
  #         vector with the realisation
  
  n <- length(calling_probs_)
  outvec <- rep(NA, length(cdr_gt_))
  ind <- runif(length(calling_probs_$p)) <= calling_probs_$p * scale_
  check <- any(ind)
  doom <- 0
  while(!check & doom <=100){
    ind <- runif(length(calling_probs_$p)) <= calling_probs_$p * scale_
    check <- any(ind)
    doom = doom + 1
    if(doom == 100) print("Calling probs too small!")
  }
  
  outvec[ind] <- cdr_gt_[ind]
  return(outvec)
}

make_gt_list <- function(cdr_gt_, masts_){
  # takes a dataframe with daily vectors of cell-ground truth and returns a 
  #   list that has the same format as the real thing
  
  # input:
  #         cdr_gt_     a dataframe/matrix with rownames of days and ncol == nt_
  #         masts_      the masts dataframe with mast_x/y
  
  # output:
  #         a list with elements for each day in the format of the ground truth
  #         from the main study
  out_list <- sapply(1:nrow(cdr_gt_), function(ii){
    nt <- ncol(cdr_gt_)
    v <- cdr_gt_[ii, ]
    data.frame(x_mean = masts_[as.character(v), "mast_x"],
               y_mean = masts_[as.character(v), "mast_y"],
               x_mast = masts_[as.character(v), "mast_x"],
               y_mast = masts_[as.character(v), "mast_y"],
               day = ii,
               day_next = ii,
               f_start = (0:(nt-1) / nt),
               f_end = (1:nt) / nt,
               stop = NA)
  }, simplify = F)
  names(out_list) <- rownames(cdr_gt_)
  return(out_list)
}

create_user_realisations <- function(n_, n_routines_, n_masts_,
                                     masts_, nt_, hour_shift_,
                                     mean1_, mean2_, lim_,
                                     min_, max_, equal_, scale_){
  # Create the "user days" for a fictitious user
  
  # input:
  #         n_          the number of desired days
  #         n_routines_ number of routines
  #         n_masts_    number of available places
  #         masts_      list of available masts
  #         nt_         length of a vector
  #         hour_shift_ functional midnight 
  #         mean1_      mean for number of stops
  #         mean2_      mean for durations
  #         lim_        minimum distance between two locations
  #         min1_       minimum number of stops in a routine
  #         max1_       maximum number of stops in a routine
  #         equal_      proportion of 1/n probabilities of calling
  #         scale_      multiplicative factor on probs
 
  
  # output:
  #         list of matrix with gt, observations and routines as well as
  #           seen masts
  
  # --- Establish the routines
  home <- sample(masts_$id_masts, 1)
  available_masts <- sample(setdiff(masts_$id_masts, home), 
                            min(nrow(masts_) - 1, n_masts_))
  check <- masts_check(available_masts, masts_, lim_)
  doom <- 0
  while(!check & doom <= 100){
    available_masts <- sample(setdiff(masts_$id_masts, home), 
                              min(nrow(masts_) - 1, n_masts_))
    check <- masts_check(available_masts, masts_, lim_)
    doom <- doom + 1
    if(doom == 100) print("No suitable set of masts was found")
  }
  
  routines <- as.list(1:(n_routines_))
  for(ii in 1:n_routines_){
    routines[[ii]] <- 
      # create_routine_2(subset(masts_, id_masts %in% c(home, available_masts)), 
      #                  nt_, mean1_, mean2_, 
      #                  home_ = home,
      #                  hour_shift_, lim_,
      #                  min_ = min_, max_ = max_)
      create_routine_3(subset(masts_, id_masts %in% c(home, available_masts)), 
                       nt_,
                       home_ = home,
                       hour_shift_, lim_)
  }
  masts_so_far <- lapply(routines[1:n_routines_], function(df){
    df$id_masts
  }) %>% do.call(what = c)
  
  # --- Create realisations
  routines_taken <- sample(n_routines_, n_, replace = T, 
                           prob = pmax(0.2, exp(-(1:n_routines_)/2)))
  simulated_days <- 
    t(sapply(routines[routines_taken], 
           function(df){
             noise <- get_noise(masts_$id_masts, count_ = 0, fixed_ = T, 
                       nt_ = nt_, hour_shift_ = hour_shift_)
             df %>% shift_routine_2() %>% 
               make_cdr_gt(home_ = home, noise_ = noise, nt_ = nt_)
           }))
  colnames(simulated_days) <- index_to_hour(1:nt_, nt_, hour_shift_)
  rownames(simulated_days) <- 1:n_
  
  # --- Create the observations
  call_probs <- get_calling_probs(nt_, hour_shift_, equal = equal_)
  call_obs <- t(apply(simulated_days, 1, get_user_day, 
                    calling_probs_ = call_probs, scale_ = scale_))
  colnames(call_obs) <- index_to_hour(1:nt_, nt_, hour_shift_)
  rownames(call_obs) <- 1:n_
  gt_formatted = make_gt_list(simulated_days, masts_)
  
  return(list(
    gt = simulated_days,
    obs = call_obs,
    routines = routines,
    seen_masts = masts_[as.character(c(home, masts_so_far)), ],
    gt_formatted = gt_formatted
  ))
}

display_routines <- function(routines_, nt_ = 24, masts_ = NULL){
  # Graphically displays routines
  
  # input:
  #         routines_   a list of routines (v2 format)
  #         nt_         length of daily vectors
  #         masts_      optional argument. if given, a map is drawn
  
  # output:
  #         none, but a plot is produced showing the routines
  library(RColorBrewer)
  masts. <- unique(lapply(routines_, function(df) df$id_masts) %>% 
                              do.call(what = c))
  cols <- colorRampPalette(c("red", "yellow", "green", "blue", "turquoise", 
                             "gray", "purple"))(length(masts.))
  plot(NULL, xlim = c(0, nt_), ylim = c(0, length(routines_)),
       xlab = "Index", ylab = "Routine Number",
       main = "Routines Visualisation")
  for(ii in 1:length(routines_)){
    routine <- routines_[[ii]]
    if(nrow(routine) > 0){
      for(jj in 1:nrow(routine)){
        rect(routine[jj, "start_index"], ii - 1, 
             routine[jj, "start_index"] + routine[jj, "duration"], ii,
             col = cols[which(routine[jj, "id_masts"] == masts.)])
      }
    }
  }
  if(!is.null(masts_)){
    return(leaflet() %>% addTiles() %>% 
      addCircleMarkers(lng = masts_[masts., "lon"], 
                       lat = masts_[masts., "lat"], 
                       col = cols))
  }
  return(NULL)
}

block_number <- function(n_, p_, m_ = NULL){
  # number integer divided by block size (optional: modulo a number)
  
  # input:
  #         n_          number whose block number is of interest
  #         p_          block size
  #         m_          modulus that the block index should be taken by
  
  # output:
  #         the p_-block index of n_ (modulo m_)
  
  # --- Establish the routines
  if(is.null(m_)){
    (n_-1)%/%p_+1
  }else{
    ((n_-1)%/%p_+1) %% m_
  }
}

