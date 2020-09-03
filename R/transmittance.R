#' Relative optical air mass
#'
#' Calculates relative optical air mass. Returns NA for negative values.
#'
#' @rdname trans_air_mass_rel
#' @param ... Additional parameters passed to later functions.
#' @return Relative optical air mass.
#' @export
#'
trans_air_mass_rel <- function (...) {
  UseMethod("trans_air_mass_rel")
}

#' @rdname trans_air_mass_rel
#' @method trans_air_mass_rel numeric
#' @param sol_elevation Solar elevation in degrees.
#' @export
#'
trans_air_mass_rel.numeric <- function(sol_elevation, ...) {
  f <- pi/180                                 # Winkel in Radiant Faktor
  mr <- 1 / (sin(sol_elevation*f) + (1.5*sol_elevation**-0.72) )
  return(ifelse(sol_elevation <= 0, NA, mr))
}

#' @rdname trans_air_mass_rel
#' @method trans_air_mass_rel weather_station
#' @param weather_station Object of class weather_station
#' @export
#'
trans_air_mass_rel.weather_station <- function(weather_station, ...){
  sol_elevation <- sol_elevation(weather_station)
  return(trans_air_mass_rel(sol_elevation))
}


#' Absolute optical air mass
#'
#' Calculates absolute optical air mass.
#'
#' @rdname trans_air_mass_abs
#' @param ... Additional parameters passed to later functions.
#' @return Absolute optical air mass.
#' @export
#'
trans_air_mass_abs <- function (...) {
  UseMethod("trans_air_mass_abs")
}

#' @rdname trans_air_mass_abs
#' @method trans_air_mass_abs numeric
#' @param air_mass_rel Relative optical air mass
#' @param p Air pressure in hPa.
#' @export
#'
trans_air_mass_abs.numeric <- function(air_mass_rel, p, ...){
  p0 <- 1013.25
  air_mass_abs <- air_mass_rel*(p/p0)
  return (air_mass_abs)
}

#' @rdname trans_air_mass_abs
#' @method trans_air_mass_abs weather_station
#' @param weather_station Object of class weather_station
#' @export
#'
trans_air_mass_abs.weather_station <- function(weather_station, ...){

  check_availability(weather_station, "p2")

  p <- weather_station$properties$p2
  air_mass_rel <- trans_air_mass_rel(weather_station)
  return(trans_air_mass_abs(air_mass_rel, p))
}


#' Transmittance due to rayleigh scattering
#'
#' Calculates transmittance due to rayleigh scattering.
#'
#' @rdname trans_rayleigh
#' @param ... Additional parameters passed to later functions.
#' @return Transmittance due to rayleigh scattering (0-1).
#' @export
#'
trans_rayleigh <- function (...) {
  UseMethod("trans_rayleigh")
}

#' @rdname trans_rayleigh
#' @method trans_rayleigh numeric
#' @param air_mass_abs Absolute optical air mass.
#' @export
#'
trans_rayleigh.numeric <- function(air_mass_abs, ...){
  x <- (-0.0903)*air_mass_abs**0.84*(1.+air_mass_abs-air_mass_abs**1.01)
  return(exp(x))
}

#' @rdname trans_rayleigh
#' @method trans_rayleigh weather_station
#' @param weather_station Object of class weather_station
#' @export
#'
trans_rayleigh.weather_station <- function(weather_station, ...){
  air_mass_abs <- trans_air_mass_abs(weather_station)
  return(trans_rayleigh(air_mass_abs))
}


#' Transmittance due to ozone
#'
#' Calculates transmittance due to ozone.
#'
#' @rdname trans_ozone
#' @param ... Additional parameters passed to later functions.
#' @return Transmittance due to ozone (0-1).
#' @export
#'
trans_ozone <- function (...) {
  UseMethod("trans_ozone")
}

#' @rdname trans_ozone
#' @method trans_ozone numeric
#' @param air_mass_rel Relative optical air mass.
#' @param oz Columnar ozone in cm. Default is average value of 0.35 cm.
#' @export
#'
trans_ozone.numeric <- function(air_mass_rel, oz = 0.35, ...) {
  x <- oz*air_mass_rel
  xx <- 0.1611*x*(1+139.48*x)**-0.3035-0.002715*x*(1+0.044*x+0.0003*x**2)**-1
  return(1.-xx)
}

#' @rdname trans_ozone
#' @method trans_ozone weather_station
#' @param weather_station Object of class weather_station
#' @export
#'
trans_ozone.weather_station <- function(weather_station, ...){
  air_mass_rel <- trans_air_mass_rel(weather_station)
  return(trans_ozone(air_mass_rel, ...))
}


#' Transmittance due to water vapor
#'
#' Calculates transmittance due to water vapor.
#'
#' @rdname trans_vapor
#' @param ... Additional parameters passed to later functions.
#' @return Transmittance due to water vapor (0-1).
#' @export
#'
trans_vapor <- function (...) {
  UseMethod("trans_vapor")
}

#' @rdname trans_vapor
#' @method trans_vapor numeric
#' @param air_mass_rel Relative optical air mass.
#' @param precipitable_water Precipitable water in cm.
#' @export
#'
trans_vapor.numeric <- function(air_mass_rel, precipitable_water, ...) {
  y <- precipitable_water*air_mass_rel
  yy <- 2.4959*y*((1+79.034*y)**0.6828+6.385*y)**-1
  return(1.-yy)
}

#' @rdname trans_vapor
#' @method trans_vapor weather_station
#' @param weather_station Object of class weather_station.
#' @export
#'
trans_vapor.weather_station <- function(weather_station, ...){
  air_mass_rel <- trans_air_mass_rel(weather_station)
  precipitable_water <- hum_precipitable_water(weather_station)
  return(trans_vapor(air_mass_rel, precipitable_water))
}


#' Transmittance due to aerosols
#'
#' Calculates transmittance due to aerosols.
#'
#' @rdname trans_aerosol
#' @param ... Additional parameters passed to later functions.
#' @return Transmittance due to aerosols (0-1).
#' @export
#'
trans_aerosol <- function (...) {
  UseMethod("trans_aerosol")
}

#' @rdname trans_aerosol
#' @method trans_aerosol numeric
#' @param air_mass_abs Absolute optical air mass.
#' @param vis Visibility in km.
#' @export
#'
trans_aerosol.numeric <- function(air_mass_abs, vis = 30, ...) {
  tau38 <- 3.6536*vis**-0.7111
  tau5 <- 2.4087*vis**-0.719
  tex <- 0.2758*tau38+0.35*tau5
  x <- (tex**0.873*(-1))*(1.+tex-tex**0.7088)*air_mass_abs**0.9108
  return(exp(x))
}

#' @rdname trans_aerosol
#' @method trans_aerosol weather_station
#' @param weather_station Object of class weather_station.
#' @export
#'
trans_aerosol.weather_station <- function(weather_station, ...){
  air_mass_abs <- trans_air_mass_abs(weather_station)
  return(trans_aerosol(weather_station, ...))
}


#' Transmittance due to gas
#'
#' Calculates transmittance due to O2 and CO2.
#'
#' @rdname trans_gas
#' @param ... Additional parameters passed to later functions.
#' @return Transmittance due to gas (0-1)
#' @export
#'
trans_gas <- function (...) {
  UseMethod("trans_gas")
}

#' @rdname trans_gas
#' @method trans_gas numeric
#' @param air_mass_abs Absolute optical air mass.
#' @export
#'
trans_gas.numeric <- function(air_mass_abs, ...) {
  return(exp(-0.0127*air_mass_abs**0.26))
}

#' @rdname trans_gas
#' @method trans_gas weather_station
#' @param weather_station Object of class weather_station.
#' @export
#'
trans_gas.weather_station <- function(weather_station, ...){
  air_mass_abs <- trans_air_mass_abs(weather_station)
  return(trans_gas(air_mass_abs))
}


#' Total transmittance
#'
#' Calculates total transmittance of the atmosphere.
#'
#' @rdname trans_total
#' @param ... Additional parameters passed to later functions.
#' @return Total transmittance (0-1)
#' @export
#'
trans_total <- function (...) {
  UseMethod("trans_total")
}

#' @rdname trans_total
#' @method trans_total numeric
#' @param sol_elevation Solar elevation in degrees.
#' @param t Air temperature in degrees C.
#' @param elev Altitude above sea level in m.
#' @param oz Columnar ozone in cm. Default is average global value.
#' @param vis Meteorological visibility in km. Default is the visibility on a clear day.
#' @param p OPTIONAL. Pressure in hPa. Estimated from elev and t if not available.
#' @export
#'
trans_total.numeric <- function(sol_elevation, t, elev,
                        oz = 0.35, vis = 30, p = NULL, ...){
  if(is.null(p)) p <- pres_p(elev, t)
  pw <- hum_precipitable_water(p, t, elev)
  mr <- trans_air_mass_rel(sol_elevation)
  ma <- trans_air_mass_abs(mr, p)
  trans_total <- data.frame(rayleigh = trans_rayleigh(ma),
                            ozone = trans_ozone(mr, oz),
                            vapor = trans_vapor(mr, pw),
                            aerosol = trans_aerosol(ma, vis),
                            gas = trans_gas(ma))
  trans_total$total <- apply(trans_total, 1, prod)
  return(trans_total$total)
}

#' @rdname trans_total
#' @method trans_total weather_station
#' @param weather_station Object of class weather_station.
#' @export
#'
trans_total.weather_station <- function(weather_station, ...){
  sol_elevation <- sol_elevation(weather_station)
  check_availability(weather_station, "t2", "z2", "elevation", "p2")
  t <- weather_station$measurements$t2
  elev <- weather_station$location_properties$elevation + weather_station$properties$z2
  p2 <- weather_station$measurements$p2
  return(trans_total(sol_elevation, t, elev, p = p2, ...))
}
