#' Roughness length
#'
#' Calculate the roughness length of an surface on the basis of the obstacle height or the type of the surface
#' Possible surface types are:
#' "Wiese", "Acker", "Gruenflaeche", "Strasse", "Landwirtschaft", "Siedlung", "Nadelwald", "Laubwald", "Mischwald", "Stadt"
#' You need to specify only one, "type" OR "obs_height".
#'
#' @param surface_type Type of surface
#' @param obs_height Height of obstacle in m.
#'
#' @return Roughness length in m.
#' @export
#'
turb_roughness_length <- function(surface_type = NULL, obs_height = NULL){
  if(is.null(obs_height)==F){z0 <- obs_height*0.1}
  else if(is.null(surface_type)==F && is.null(obs_height)){
    z0 <- surface_properties[which(surface_properties$surface_type==surface_type),]$roughness_length
  }
  else if(is.null(surface_type)==F && is.null(obs_height)==F){z0 <- NA}
  return(z0)
}

#' Displacement height
#'
#' Calculate the displacement height, caused by an obstacle (e.g. a crop field).
#'
#' @param obs_height Height of obstacle in m.
#'
#' @return Displacement height in m.
#' @export
#'
turb_displacement <- function(obs_height){
  d0 <- (2/3)*obs_height      # for Vegetation only
  return(d0)
}

#' Friction velocity
#'
#' Calculate the friction velocity of the underground.
#'
#' @param v Windspeed in height of anemometer in m/s.
#' @param z Height of anemometer in m.
#' @param z0 Roughness length in m.
#'
#' @return Friction velocity in m/s.
#' @export
#'
turb_ustar <- function(v, z, z0){
  ustar <- v*0.4/log(z/z0)
  return(ustar)
}
