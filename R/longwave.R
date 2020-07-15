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
lw_emissivity_air <- function(temp, altitude, air_pressure = NULL){
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
lw_surface <- function(temp, emissivity = 0.95){
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
lw_atmospheric <- function(emissivity_air, temp){
  sigma <- 5.670374 * 10^-8
  gs <- emissivity_air*sigma*(temp+273.15)**4
  return(gs)
}
