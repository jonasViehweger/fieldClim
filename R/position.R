#' Minimum Distance between climate station and obstacle
#'
#' Calculate the needed minimum distance between climate station and obstacle (e.g. forest), to ensure independence of measurements.
#'
#' @rdname pos_min_dist
#' @param ... Additional parameters passed to later functions.
#' @return Minimum distance in m.
#' @export
#'
pos_min_dist <- function (...) {
  UseMethod("pos_min_dist")
}

#' @rdname pos_min_dist
#' @method pos_min_dist numeric
#' @param obs_width Width of obstacle in m.
#' @param obs_height Height of obstacle in m.
#' @param ring True, if obstacle is circularly surrounded by the obstacle.
#' @param obs_radius If ring = T: radius of the ring in m.
#'
#' @return
#'
pos_min_dist.numeric <- function(obs_width, obs_height, ring = F, obs_radius = NULL, ...){
  #if climate station is positioned on a clearing:
  if(ring == T){
    min_dist <- pi*obs_radius+10*obs_height
  }
  #else, if climate station is positioned behind a forest:
  else{
    #check if height > width
    if(obs_height > obs_width){
      min_dist <- 0.5*obs_height+10*obs_width
    }
    #check if height < width
    else if(obs_height < obs_width){
      min_dist <- 0.5*obs_width+10*obs_height
    }
    #check if height ~ width (height = width+-5%)
    else if(obs_height <= obs_width*1.05 && obs_height >= obs_width*0.95){
      min_dist <- 5*(obs_height+obs_width)
    }
  }
}

















#' Maximum distance between climate station and obstacle.
#'
#' If station is positioned beyond minimum distance, check if it lies within maximum distance.
#'
#' @param dist Distance between climate station and obstacle (e.g. forest) in m.
#' @param obs_width Width of obstacle in m.
#' @param obs_height Height of obstacle in m.
#' @param ring True, if obstacle is circularly surrounded by the obstacle.
#'
#' @return Message, that tells you if climate station is well positioned or, if not, in which distance to the obstacle it needs to be placed instead.
#' @export
#'
pos_max_dist <- function(dist, obs_width, obs_height, ring = F){
  if(ring == T){
    if(dist < 15*obs_height){
      return("The climate station is postioned well.")
    }else{
      max_dist <- 15*obs_height
      return(paste("The climate station is positioned too far from the obstacle. It needs to be placed in a position, that is closer than", max_dist, "m from the obstacle."))
    }
  }
  #else, if climate station is positioned behind a forest:
  else{
    #check if height > width
    if(obs_height > obs_width){
      if(dist < 15*obs_width){
        return("The climate station is postioned well.")
      }else{
        max_dist <- round(15*obs_width, digits = 2)
        return(paste("The climate station is positioned too far from the obstacle. It needs to be placed in a position, that is closer than", max_dist, "m from the obstacle."))
      }
    }
    #check if height < width
    else if(obs_height < obs_width){
      if(dist < 15*obs_height){
        return("The climate station is postioned well.")
      }else{
        max_dist <- round(15*obs_height, digits = 2)
        return(paste("The climate station is positioned too far from the obstacle. It needs to be placed in a position, that is closer than", max_dist, "m from the obstacle."))}
    }
  }
}

#' Necessary Anemometer height.
#'
#' Checks if distance of climate station to the forest is smaller than minimum distance. If so, calulates in which height the anemometer needs to be
#' positioned to ensure independency of measurements.
#'
#' @param dist Distance between climate station and obstacle (e.g. forest) in m.
#' @param min_dist Minimum distance between climate station and obstacle, that is needed to ensure independency of measurements, in m.
#' @param obs_height Height of obstacle in m.
#'
#' @return Message, that tells you if climate station is well positioned or, if not, in which height the anemometer needs to be positioned to ensure independency of measurements.
#' @export
#'
pos_anemometer_height <- function(dist, min_dist, obs_height){
  if(dist >= min_dist){
    return("The climate station is positioned beyond the needed minimum distance. It is not required to change the height of the anemometer.")
  }
  if(dist < min_dist){
    repos <- round(obs_height*(min_dist-dist)/min_dist, 2)
    return(paste("The climate station is positioned too close to the obstacle. It needs to repositioned", repos, "m higher."))
  }
}
