#' Roughness length
#'
#' Calculate the roughness length of an surface on the basis of the obstacle height or the type of the surface
#' Possible surface types are:
#' "Wiese", "Acker", "Gruenflaeche", "Strasse", "Landwirtschaft", "Siedlung", "Nadelwald", "Laubwald", "Mischwald", "Stadt"
#' You need to specify only one, "type" OR "obs_height".
#'
#' @rdname turb_roughness_length
#' @param ... Additional parameters passed to later functions.
#' @return Pressure in hPa.
#' @export
#'
turb_roughness_length <- function (...) {
  UseMethod("turb_roughness_length")
}

#' @rdname turb_roughness_length
#' @method turb_roughness_length default
#' @param surface_type Type of surface
#' @param obs_height Height of obstacle in m.
#' @export
turb_roughness_length.default <- function(surface_type = NULL, obs_height = NULL, ...){
  surface_properties <- surface_properties
  if(!is.null(obs_height)){
    z0 <- obs_height*0.1
  } else if(!is.null(surface_type)) {
    z0 <- surface_properties[which(surface_properties$surface_type==surface_type),]$roughness_length
  } else {
    z0 <- NA
  }
  return(z0)
}

#' @rdname turb_roughness_length
#' @method turb_roughness_length weather_station
#' @param weather_station Object of class weather_station
#' @export
turb_roughness_length.weather_station <- function(weather_station, ...){
  check_availability(weather_station, "obs_height", "surface_type")
  obs_height <- weather_station$location_properties$obs_height
  surface_type <- weather_station$location_properties$surface_type
  if(is.null(obs_height) & is.null(surface_type)){
    stop("Either surface_type or obs_height must be set.")
  }
  return(turb_roughness_length(surface_type, obs_height))
}


#' Displacement height
#'
#' Calculate the displacement height, caused by an obstacle (e.g. a crop field).
#' Works for vegetation only.
#'
#' @rdname turb_displacement
#' @param ... Additional parameters passed to later functions.
#' @return Displacement height in m.
#' @export
#'
turb_displacement <- function (...) {
  UseMethod("turb_displacement")
}

#' @rdname turb_displacement
#' @method turb_displacement numeric
#' @param obs_height Height of vegetation in m.
#' @export
turb_displacement.numeric <- function(obs_height, ...){
  d0 <- (2/3)*obs_height      # for Vegetation only
  return(d0)
}

#' @rdname turb_displacement
#' @method turb_displacement weather_station
#' @param weather_station Object of class weather_station
#' @export
turb_displacement.weather_station <- function(weather_station, ...){
  check_availability(weather_station, "obs_height")
  obs_height <- weather_station$location_properties$obs_height
  return(turb_displacement(obs_height))
}


#' Friction velocity
#'
#' Calculate the friction velocity of the underground.
#'
#' @rdname turb_ustar
#' @param ... Additional parameters passed to later functions.
#' @return Friction velocity in m/s.
#' @export
#'
turb_ustar <- function (...) {
  UseMethod("turb_ustar")
}

#' @rdname turb_ustar
#' @method turb_ustar numeric
#' @param v Windspeed in height of anemometer in m/s.
#' @param z Height of anemometer in m.
#' @param z0 Roughness length in m.
#' @export
turb_ustar.numeric <- function(v, z, z0, ...){
  ustar <- (v*0.4)/log(z/z0)
  return(ustar)
}

#' @rdname turb_ustar
#' @method turb_ustar weather_station
#' @param weather_station Object of class weather_station
#' @export
turb_ustar.weather_station <- function(weather_station, ...){
  check_availability(weather_station, "v1", "z1")
  v <- weather_station$measurements$v1
  z <- weather_station$properties$z1
  z0 <- turb_roughness_length(weather_station)
  return(turb_ustar(v,z,z0))
}
