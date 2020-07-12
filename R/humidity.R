#' Saturation vapor pressure
#'
#' Calculates the saturation vapor pressure from air temperature using the Magnus
#' formula (applicable over water surfaces).
#'
#' @param temp numeric. Air temperature in degrees Celsius
#'
#' @return numeric. Saturation vapor pressure in hPa
#' @export
#'
#' @examples
hum_sat_vapor_pres <- function(temp) {
  a <- 7.5
  b <- 235
  return(6.1078*10**((a*temp)/(b+temp)))
}


#' Vapor pressure
#'
#' Calculates vapor pressure from relative humidity and saturation vapor pressure
#'
#' @param hum_rel numeric. Relative humidity in percent (0-100)
#' @param sat_vapor_pres numeric. Saturation vapor pressure in hPa
#'
#' @return numeric. Vapor pressure in hPa
#' @export
#'
#' @examples
hum_vapor_pres <- function(hum_rel, sat_vapor_pres){
  return((hum_rel/100)*sat_vapor_pres)
}

#' Specific humidity
#'
#' Calculates specfic humidity from vapor pressure and air pressure.
#'
#' @param vapor_pressure numeric. Vapor pressure in hPa
#' @param air_pressure numeric. Air pressure in hPa
#'
#' @return numeric. Specific humidity in kg/kg
#' @export
#'
#' @examples
hum_specific <- function(vapor_pressure, air_pressure) {
  return(0.622*(vapor_pressure/air_pressure))
}

#' Absolute humidity
#'
#' Calculates absolute humidity from vapor pressure and temperature.
#'
#' @param vapor_pressure numeric. Vapor presure in hPa
#' @param temp numeric. Air temperature in degrees celsius
#'
#' @return numeric. Absolute humidity in kg/m^3
#' @export
#'
#' @examples
hum_absolute <- function(vapor_pressure, temp) {
  return(0.21668*vapor_pressure)/temp
}

# Vewrdunstungsw?rme in J/kg, temp (x1) in ?C
#' Heat of evaporation
#'
#' @param temp numeric. Air temperature in degrees celsius.
#'
#' @return
#' @export
#'
#' @examples
hum_evap_heat <- function(temp){
  return((2.5008-0.002372*temp)*10^6)
}
