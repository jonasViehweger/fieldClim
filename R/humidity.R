#' Saturation vapor pressure
#'
#' Calculates the saturation vapor pressure from air temperature using the Magnus
#' formula (applicable over water surfaces).
#'
#' @param temp Air temperature in degrees C.
#'
#' @return Saturation vapor pressure in hPa.
#' @export
#'
hum_sat_vapor_pres <- function(temp) {
  a <- 7.5
  b <- 235
  return(6.1078*10**((a*temp)/(b+temp)))
}


#' Vapor pressure
#'
#' Calculates vapor pressure from relative humidity and saturation vapor pressure
#'
#' @param hum Relative humidity in %.
#' @param temp Air temperature in degrees C.
#'
#' @return Vapor pressure in hPa.
#' @export
#'
hum_vapor_pres <- function(hum, temp){
  sat_vapor_pres <- hum_sat_vapor_pres(temp)
  return((hum/100)*sat_vapor_pres)
}

#' Specific humidity
#'
#' Calculates specfic humidity from vapor pressure and air pressure.
#'
#' @param vapor_pressure Vapor pressure in hPa.
#' @param air_pressure Air pressure in hPa.
#'
#' @return Specific humidity in kg/kg.
#' @export
#'
hum_specific <- function(vapor_pressure, air_pressure) {
  return(0.622*(vapor_pressure/air_pressure))
}

#' Absolute humidity
#'
#' Calculates absolute humidity from vapor pressure and temperature.
#'
#' @param vapor_pressure Vapor presure in hPa.
#' @param temp Air temperature in degrees celsius.
#'
#' @return Absolute humidity in kg/m^3.
#' @export
#'
hum_absolute <- function(vapor_pressure, temp) {
  return(0.21668*vapor_pressure)/temp
}

#' Heat of evaporation
#'
#' Calculates heat of evaporation for water from air temperature.
#'
#' @param temp Air temperature in degrees C.
#'
#' @return Heat of evaporation in J/kg.
#' @export
#'
hum_evap_heat <- function(temp){
  return((2.5008-0.002372*temp)*10^6)
}

#' Moisture gradient
#'
#' Calculates moisture gradient.
#'
#' @param hum1 Relative humidity at lower height in %.
#' @param hum2 Relative humidity at upper height in %.
#' @param t1 Air temperature at lower height in degrees C.
#' @param t2 Air temperature at upper height in degrees C.
#' @param p1 Air pressure at lower height in hPa.
#' @param p2 Air pressure at lower height in hPa.
#' @param z1 Lower measurement height in m.
#' @param z2 Upper measurement height in m.
#'
#' @return
#' @export
#'
hum_moisture_gradient <- function(hum1, hum2, t1, t2, p1, p2, z1, z2){
  # vapor pressure
  vp1 <- hum_vapor_pres(hum1, t1)
  vp2 <- hum_vapor_pres(hum2, t2)

  # specific humidity
  sh1 <- hum_specific(vp1, p1)
  sh2 <- hum_specific(vp2, p1)
  return((sh2-sh1) / log(z2-z1))
}
