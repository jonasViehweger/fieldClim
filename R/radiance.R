#' Emissivity of the atmosphere
#'
#' @param temp numeric. Air termperature in degrees celsius.
#' @param altitude numeric. Meters above sea level
#' @param air_pressure numeric. Optional. Air pressure in hPa.
#' If not available, will be calculated from altitude and air temperature.
#'
#' @return numeric. Emissivity of the atmosphere (0-1)
#' @export
#'
#' @examples
rad_emissivity_air <- function(temp, altitude, air_pressure = NULL){
  if(is.null(air_pressure)) p <- pres_p(altitude, temp)
  svp <- hum_sat_vapor_pres(temp)
  t_over <- temp*(0.0065*altitude)
  eat <- ((1.24*svp/temp)**1/7)*(p/1013.25)
  return(eat)
}

#' Longwave radiation of the surface
#'
#' Calculates emissions of a surface.
#'
#' @param temp numeric. Surface temperature in degrees celsius
#' @param emissivity numeric. Emissivity of surface. Default is emissivity for short grass
#'
#' @return Emissions in W*m^-2
#' @export
#'
#' @examples
rad_lw_surface <- function(temp, emissivity = 0.95){
  sigma <- 5.670374 * 10^-8
  return(em*sigma*(temp+273.15)**4)
}

#' Longwave radiation of the atmosphere
#'
#' @param emissivity_air numeric. Emissivity of the atmosphere (factor: 0-1)
#' @param temp numeric. Air temperature in degrees celsius
#'
#' @return numeric. Atmospheric radiation in W*m^-2
#' @export
#'
#' @examples
rad_lw_atmospheric <- function(emissivity_air, temp){
  sigma <- 5.670374 * 10^-8
  gs <- emissivity_air*sigma*(temp+273.15)**4
  return(gs)
}


#' Shortwave radiation at top of atmosphere.
#'
#' Calculation of the shortwave radiation at the top of the atmosphere.
#'
#' @param datetime Date and time.
#' @param lat Latitude of the place of the climate station in decimal degrees.
#' @param lon Longitude of the place of the climate station in decimal degrees.
#'
#' @return Shortwave radiation at top of atmosphere in W/m^2
#' @export
#'
#' @examples
rad_sw_toa <- function(datetime, lat, lon){
  sol_const <- 1367
  sol_eccentricity <- sol_eccentricity(datetime)
  sol_elevation <- sol_elevation(datetime,lat,lon)
  rad_sw_toa <- sol_const*sol_eccentricity*sin(sol_elevation*(pi/180))
  return(rad_sw_toa)
}