#' Emissivity of the atmosphere
#'
#' Calculates emissivity of the atmosphere.
#'
#' @param temp Air temperature in degrees C.
#' @param elev Elevation above sea level in m.
#' @param p Air pressure in hPa.
#'
#' @return Emissivity of the atmosphere (0-1).
#' @export
#'
lw_emissivity_air <- function(temp, elev, p){
  svp <- hum_sat_vapor_pres(temp)
  t_over <- temp*(0.0065*elev)
  eat <- ((1.24*svp/temp)**1/7)*(p/1013.25)
  return(eat)
}

#' Longwave radiation of the surface
#'
#' Calculates emissions of a surface.
#'
#' @param temp Surface temperature in degrees C.
#' @param emissivity Emissivity of surface (0-1). Default is emissivity for short grass.
#'
#' @return Surface emissions in W/m^2.
#' @export
#'
lw_surface <- function(temp, emissivity = 0.95){
  sigma <- 5.670374 * 10^-8
  return(em*sigma*(temp+273.15)**4)
}

#' Longwave radiation of the atmosphere
#'
#' @param emissivity_air Emissivity of the atmosphere (0-1).
#' See \code{\link{lw_emmissivity_air}} for calculation.
#' @param temp Air temperature in degrees C.
#'
#' @return Atmospheric radiation in W/m^2.
#' @export
#'
lw_atmospheric <- function(temp, emissivity_air){
  sigma <- 5.670374 * 10^-8
  gs <- emissivity_air*sigma*(temp+273.15)**4
  return(gs)
}
