# Funktion Emissionsfaktor Luft
# steam_pressure (hPa), Temperature_atmosphere (?C), air_pressure (hPa), z: altitude ground
lw_emissivity_air <- function(temp, altitude, air_pressure = NULL){
  if(is.null(air_pressure)) p <- pres_p(altitude, temp)
  svp <- hum_sat_vapor_pres(temp)
  t_over <- temp*(0.0065*altitude)
  eat <-((1.24*svp/temp)**1/7)*(p/1013.25);
  return(eat)
}

#' Longwave terrestric emission
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
lw_surface <- function(temp, emissivity=0.95){
  sigma <- 5.670374 * 10^-8
  return(em*sigma*(temp+273.15)**4)
}

# Funktion amtosph?rische Gegenstrahlung
# eat: emmissionsfaktor luft
# o:   stefan-boltzmann-konstant (whhy)
# tat: temperatur atmosphäre
lw_atmospheric <- function(emissivity_air, temp){
  sigma <- 5.670374 * 10^-8
  gs <- emissivity_air*sigma*temp**4
  return(gs)
}
