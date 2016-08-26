dist_from_locs <- function(locs_){
  # gets travelled distances from 2-column matrix locs (in projected coords)
  
  # input:
  #         locs_       2-column matrix with x and y coordinates
  
  # output:
  #         travelled distance (a number)
  sum(sqrt(apply(apply(locs_, 2, FUN = diff)^2, 1, FUN = sum)))
}

get_dist_travelled <- function(locations_, masts_){
  # gets travelled distances from locations and masts
  
  # input:
  #         locations_  data.frame/matrix with predicted masts per time slot
  #         masts_      masts dataframe (with x and y as projected coordinates)
  
  # output:
  #         vector with distances travelled
  as.numeric(apply(locations_, 1, FUN = function(v){
    dist_from_locs(masts_[v, c("mast_x", "mast_y")])
  }))
}

rog_from_locs <- function(locs_){
  # gets RoG from 2-column matrix locs (in projected coords)
  
  # input:
  #         locs_       2-column matrix with x and y coordinates
  
  # output:
  #         Radius of Gyration (a number)
  centre <- colSums(locs_) / nrow(locs_)
  mean(sqrt(apply((locs_ - rep(centre, each = nrow(locs_)))^2, 1, FUN = sum)))
}

get_rog <- function(locations_, masts_){
  # gets radius of gyration from locations and masts
  
  # input:
  #         locations_  data.frame/matrix with predicted masts per time slot
  #         masts_      masts dataframe (with x and y as projected coordinates)
  
  # output:
  #         vector with the daily radii of gyration
  as.numeric(apply(locations_, 1, FUN = function(v){
    rog_from_locs(masts_[v, c("mast_x", "mast_y")])
  }))
}














