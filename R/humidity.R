#' Saturation vapor pressure
#'
#' Calculates the saturation vapor pressure from air temperature using the Magnus
#' formula (applicable over water surfaces).
#'
#' @param t Air temperature in degrees C.
#'
#' @return Saturation vapor pressure in hPa.
#' @export
#'
hum_sat_vapor_pres <- function(t) {
  a <- 7.5
  b <- 235
  return(6.1078*10**((a*t)/(b+t)))
}


#' Vapor pressure
#'
#' Calculates vapor pressure from relative humidity and saturation vapor pressure
#'
#' @param hum Relative humidity in %.
#' @param t Air temperature in degrees C.
#'
#' @return Vapor pressure in hPa.
#' @export
#'
hum_vapor_pres <- function(hum, t){
  sat_vapor_pres <- hum_sat_vapor_pres(t)
  return((hum/100)*sat_vapor_pres)
}

#' Specific humidity
#'
#' Calculates specfic humidity from vapor pressure and air pressure.
#'
#' @param p_vapor Vapor pressure in hPa.
#' @param p Air pressure in hPa.
#'
#' @return Specific humidity in kg/kg.
#' @export
#'
hum_specific <- function(p_vapor, p) {
  return(0.622*(p_vapor/p))
}

#' Absolute humidity
#'
#' Calculates absolute humidity from vapor pressure and temperature.
#'
#' @param p_vapor Vapor presure in hPa.
#' @param t Potential air temperature in Kelvin.
#'
#' @return Absolute humidity in kg/m^3.
#' @export
#'
hum_absolute <- function(p_vapor, t_pot) {
  return((0.21668*p_vapor)/t_pot)
}

#' Enthaly of vaporization
#'
#' Calculates heat of evaporation for water from air temperature.
#'
#' @param t Air temperature in degrees C.
#'
#' @return Enthaly of vaporization in J/kg.
#' @export
#'
hum_evap_heat <- function(t){
  return((2.5008-0.002372*t)*10^6)
}


#' Total precipitable water
#'
#' Estimates total precipitable water in the atmosphere.
#' It uses a moist adiabatic temperature gradient which might not be
#' suitable for every application.
#'
#' @param p Air pressure in hPa.
#' @param t Air temperature in degrees C.
#' @param elev Elevation above sea level in m.
#'
#' @return Total precipitable water in cm (grams).
#' @export
#'
hum_precipitable_water <- function(p, t, elev){
  p0 <- 1013.25                # Pressure Standaratmosphere
  t <- t+273.15               # degrees C in K
  cof <- (elev/100) * 0.6       # average moist adiabatic T-gradient, might have to be adjusted
  t0 <- t + cof
  pw_st <- 0.0000004*exp(0.0538*t0)
  pw <- pw_st*(p/p0) *(t0/t)**0.5
  return(pw)
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
hum_moisture_gradient <- function(hum1, hum2, t1, t2, p1, p2, z1 = 2, z2 = 10){
  # saturation vapor pressure

  # vapor pressure
  vp1 <- hum_vapor_pres(hum1, t1)
  vp2 <- hum_vapor_pres(hum2, t2)

  # specific humidity
  sh1 <- hum_specific(vp1, p1)
  sh2 <- hum_specific(vp2, p2)
  return((sh2-sh1) / log(z2-z1))
}
