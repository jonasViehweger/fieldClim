
#' Roughness length.
#'
#' Calculate the roughness length of an underground on the basis of the obstacle height or the type of the underground.
#' Possible types of underground are:
#' "Wiese", "Weide", "Acker", "Sportplatz", "Strasse", "Gruenflaeche", "Landwirtschaft", "Siedlung", "Nadelwald", "Laubwald", "Mischwald", "Stadt"
#' You need to specify only one, "type" OR "obs_height".
#'
#' @param type Type of underground.
#' @param obs_height Height of obstacle in m.
#'
#' @return Roughness length in m.
#' @export
#'
#' @examples
turb_roughness_length <- function(type = NULL, obs_height = NULL){
  if((is.null(type) && is.null(obs_height)) || (is.null(type)==F && is.null(obs_height)==F)){
    return("Please specify the type of the underground OR the obs_height")
  }
  if(is.null(type)==F){
    if(type == "Wiese" || type == "Weide"){z0 = 0.02}
    else if(type == "Acker" || type == "Sportplatz"){z0 = 0.05}
    else if(type == "Strasse" || type == "Gruenflaeche" || type == "Landwirtschaft"){z0 = 0.2}
    else if(type == "Siedlung" || type == "Nadelwald"){z0 = 1.0}
    else if(type == "Laubwald" || type == "Mischwald"){z0 = 1.5}
    else if(type == "Stadt"){z0 = 2.0}}
  else if(is.null(obs_height)==F){z0 <- obs_height*0.1}
  return(z0)                    
}

#' Displacement height.
#' 
#' Calculate the displacement height, caused by an obstacle (e.g. a crop field).
#'
#' @param obs_height Height of obstacle in m.
#'
#' @return Displacement height in m.
#' @export
#'
#' @examples
turb_displacement <- function(obs_height){ 
  d0 <- (2/3)*obs_height;      # for Vegetation only
  return(d0)
}

#' Friction velocity.
#' 
#' Calculate the friction velocity of the underground.
#'
#' @param v1 Windspeed in height of anemometer in m/s.
#' @param z1 Height of anemometer in m.
#' @param z0 Roughness length in m.
#'
#' @return Friction velocity in m/s.
#' @export
#'
#' @examples
turb_ustar <- function(v1, z1, z0){
  ustar <- v1*0.4/log(z1/z0)
  return(ustar)
}