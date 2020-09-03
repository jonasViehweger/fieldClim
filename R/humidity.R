#' Saturation vapor pressure
#'
#' Calculates the saturation vapor pressure from air temperature using the Magnus
#' formula (applicable over water surfaces).
#'
#' @param ... Additional parameters passed to later functions.
#' @return Saturation vapor pressure in hPa.
#' @export
#'
hum_sat_vapor_pres <- function (...) {
  UseMethod("hum_sat_vapor_pres")
}

#' @rdname hum_sat_vapor_pres
#' @method hum_sat_vapor_pres numeric
#' @export
#' @param t Air temperature in degrees C.
hum_sat_vapor_pres.numeric <- function(t, ...) {
  a <- 7.5
  b <- 235
  return(6.1078*10**((a*t)/(b+t)))
}

#' @rdname hum_sat_vapor_pres
#' @method hum_sat_vapor_pres weather_station
#' @export
#' @param weather_station Object of class weather_station.
#' @param height Height of measurement. "lower" or "upper".
hum_sat_vapor_pres.weather_station <- function(weather_station, height = "lower", ...) {
  check_availability(weather_station, "t1", "t2")
  if(!height %in% c("upper", "lower")){
    stop("'height' must be either 'lower' or 'upper'.")
  }
  height_num <- which(height == c("lower", "upper"))
  t <- weather_station$measurements[[paste0("t", height_num)]]
  return(hum_sat_vapor_pres(t))
}


#' Vapor pressure
#'
#' Calculates vapor pressure from relative humidity and saturation vapor pressure
#'
#' @param ... Additional parameters passed to later functions.
#' @return Vapor pressure in hPa.
#' @export
#'
hum_vapor_pres <- function (...) {
  UseMethod("hum_vapor_pres")
}

#' @rdname hum_vapor_pres
#' @method hum_vapor_pres numeric
#' @export
#' @param hum Relative humidity in %.
#' @param t Air temperature in degrees C.
hum_vapor_pres.numeric <- function(hum, t, ...){
  sat_vapor_pres <- hum_sat_vapor_pres(t)
  return((hum/100)*sat_vapor_pres)
}

#' @rdname hum_vapor_pres
#' @method hum_vapor_pres weather_station
#' @export
#' @param weather_station Object of class weather_station.
#' @param height Height of measurement. "lower" or "upper".
hum_vapor_pres.weather_station <- function(weather_station, height = "lower", ...) {
  check_availability(weather_station, "t1", "t2", "hum1", "hum2")
  if(!height %in% c("upper", "lower")){
    stop("'height' must be either 'lower' or 'upper'.")
  }
  height_num <- which(height == c("lower", "upper"))
  t <- weather_station$measurements[[paste0("t", height_num)]]
  hum <- weather_station$measurements[[paste0("hum", height_num)]]
  return(hum_vapor_pres(hum, t))
}

#' Specific humidity
#'
#' Calculates specfic humidity from vapor pressure and air pressure.
#'
#' @param ... Additional parameters passed to later functions.
#' @return Specific humidity in kg/kg.
#' @export
#'
hum_specific <- function (...) {
  UseMethod("hum_specific")
}

#' @rdname hum_specific
#' @method hum_specific numeric
#' @export
#' @param p_vapor Vapor pressure in hPa.
#' @param p Air pressure in hPa.
hum_specific.numeric <- function(p_vapor, p, ...) {
  return(0.622*(p_vapor/p))
}

#' @rdname hum_specific
#' @method hum_specific weather_station
#' @export
#' @param weather_station Object of class weather_station.
#' @param height Height of measurement. "lower" or "upper".
hum_specific.weather_station <- function(weather_station, height, ...) {
  check_availability(weather_station, "p1", "p2")
  if(!height %in% c("upper", "lower")){
    stop("'height' must be either 'lower' or 'upper'.")
  }

  height_num <- which(height == c("lower", "upper"))
  p <- weather_station$measurements[[paste0("p", height_num)]]
  p_vapor <- hum_vapor_pres(weather_station, height)

  return(hum_specific(p_vapor, p))
}

#' Absolute humidity
#'
#' Calculates absolute humidity from vapor pressure and temperature.
#'
#' @param ... Additional parameters passed to later functions.
#' @return Absolute humidity in kg/m^3.
#' @export
#'
hum_absolute <- function (...) {
  UseMethod("hum_absolute")
}

#' @rdname hum_absolute
#' @method hum_absolute numeric
#' @export
#' @param p_vapor Vapor presure in hPa.
#' @param t_pot Potential air temperature in Kelvin.
hum_absolute.numeric <- function(p_vapor, t_pot, ...) {
  return((0.21668*p_vapor)/t_pot)
}

#' @rdname hum_absolute
#' @method hum_absolute weather_station
#' @export
#' @param weather_station Object of class weather_station.
#' @param height Height of measurement. "lower" or "upper".
hum_absolute.weather_station <- function(weather_station, height, ...) {
  if(!height %in% c("upper", "lower")){
    stop("'height' must be either 'lower' or 'upper'.")
  }

  t_pot <- temp_pot_temp(weather_station, height)
  p_vapor <- hum_vapor_pres(weather_station, height)

  return(hum_absolute(p_vapor, t_pot))
}

#' Enthaly of vaporization
#'
#' Calculates heat of evaporation for water from air temperature.
#'
#' @param ... Additional parameters passed to later functions.
#' @return Enthaly of vaporization in J/kg.
#' @export
#'
hum_evap_heat <- function (...) {
  UseMethod("hum_evap_heat")
}

#' @rdname hum_evap_heat
#' @method hum_evap_heat numeric
#' @export
#' @param t Air temperature in degrees C.
hum_evap_heat.numeric <- function(t, ...){
  return((2.5008-0.002372*t)*10^6)
}

#' @rdname hum_evap_heat
#' @method hum_evap_heat weather_station
#' @export
#' @param weather_station Object of class weather_station.
#' @param height Height of measurement. "lower" or "upper".
hum_evap_heat.weather_station <- function(weather_station, height = "lower", ...) {
  check_availability(weather_station, "t1", "t2")
  if(!height %in% c("upper", "lower")){
    stop("'height' must be either 'lower' or 'upper'.")
  }

  height_num <- which(height == c("lower", "upper"))
  t <- weather_station$measurements[[paste0("t", height_num)]]

  return(hum_evap_heat(t))
}


#' Total precipitable water
#'
#' Estimates total precipitable water in the atmosphere.
#' It uses a moist adiabatic temperature gradient which might not be
#' suitable for every application.
#'
#' @param ... Additional parameters passed to later functions.
#' @return Total precipitable water in cm (grams).
#' @export
#'
hum_precipitable_water <- function (...) {
  UseMethod("hum_precipitable_water")
}

#' @rdname hum_precipitable_water
#' @method hum_precipitable_water numeric
#' @export
#' @param p Air pressure in hPa.
#' @param t Air temperature in degrees C.
#' @param elev Elevation above sea level in m.
hum_precipitable_water.numeric <- function(p, t, elev, ...){
  p0 <- 1013.25                # Pressure Standaratmosphere
  t <- t+273.15               # degrees C in K
  cof <- (elev/100) * 0.6       # average moist adiabatic T-gradient, might have to be adjusted
  t0 <- t + cof
  pw_st <- 0.0000004*exp(0.0538*t0)
  pw <- pw_st*(p/p0) *(t0/t)**0.5
  return(pw)
}

#' @rdname hum_precipitable_water
#' @method hum_precipitable_water weather_station
#' @export
#' @param weather_station Object of class weather_station.
#' @param height Height of measurement. "lower" or "upper".
hum_precipitable_water.weather_station <- function(weather_station, height = "lower", ...) {
  check_availability(weather_station, "t1", "t2", "p1", "p2", "elev")
  if(!height %in% c("upper", "lower")){
    stop("'height' must be either 'lower' or 'upper'.")
  }

  height_num <- which(height == c("lower", "upper"))
  t <- weather_station$measurements[[paste0("t", height_num)]]
  p <- weather_station$measurements[[paste0("p", height_num)]]
  elev <- weather_station$location_properties$elevation

  return(hum_precipitable_water(p, t, elev))
}


#' Moisture gradient
#'
#' Calculates moisture gradient.
#'
#' @param ... Additional parameters passed to later functions.
#' @return Moisture gradient.
#' @export
#'
hum_moisture_gradient <- function (...) {
  UseMethod("hum_moisture_gradient")
}

#' @rdname hum_moisture_gradient
#' @method hum_moisture_gradient numeric
#' @export
#' @param hum1 Relative humidity at lower height in %.
#' @param hum2 Relative humidity at upper height in %.
#' @param t1 Air temperature at lower height in degrees C.
#' @param t2 Air temperature at upper height in degrees C.
#' @param p1 Air pressure at lower height in hPa.
#' @param p2 Air pressure at lower height in hPa.
#' @param z1 Lower measurement height in m.
#' @param z2 Upper measurement height in m.
hum_moisture_gradient.numeric <- function(hum1, hum2, t1, t2, p1, p2, z1 = 2, z2 = 10, ...){
  # saturation vapor pressure

  # vapor pressure
  vp1 <- hum_vapor_pres(hum1, t1)
  vp2 <- hum_vapor_pres(hum2, t2)

  # specific humidity
  sh1 <- hum_specific(vp1, p1)
  sh2 <- hum_specific(vp2, p2)
  return((sh2-sh1) / log(z2-z1))
}

#' @rdname hum_moisture_gradient
#' @method hum_moisture_gradient weather_station
#' @param weather_station Object of class weather_station
#' @export
hum_moisture_gradient.weather_station <- function(weather_station, ...){
  check_availability(weather_station, "z1", "z2", "t1", "t2", "p1", "p2", "hum1", "hum2")
  hum1 <- weather_station$measurements$hum1
  hum2 <- weather_station$measurements$hum2
  t1 <- weather_station$measurements$t1
  t2 <- weather_station$measurements$t2
  z1 <- weather_station$properties$z1
  z2 <- weather_station$properties$z2
  p1 <- weather_station$measurements$p1
  p2 <- weather_station$measurements$p2
  return(hum_moisture_gradient(hum1, hum2, t1, t2, p1, p2, z1 = 2, z2 = 10))
}
