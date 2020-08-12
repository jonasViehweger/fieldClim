pres_p <- function (...) {
  UseMethod("pres_p")
}


#' Air pressure
#'
#' Calculation of pressure as a function of height.
#'
#' @param weather_station Object of class weather_station
#' @param height lower or upper
#'
#' @return Pressure in hPa.
#' @export
#'
pres_p.weather_station <- function(weather_station, height){
  if(height=="lower"){
    t <- weather_station$measurements$t1+273.15   # to Kelvin
    z <- weather_station$location_properties$elevation + weather_station$properties$z1
  } else if(height=="upper"){
    t <- weather_station$measurements$t2+273.15   # to Kelvin
    z <- weather_station$location_properties$elevation + weather_station$properties$z2
  }
  return(pres_p(z, t))
}

#' Air pressure
#'
#' Calculation of pressure as a function of height.
#'
#' @param elev Elevation above sea level in m.
#' @param t Temperature in degrees C.
#'
#' @return Pressure in hPa.
#' @export
#'
pres_p.numeric <- function(elev, t){
  t <- t+273.15   # to Kelvin
  p0 <- 1013.25    # Standardruck in hPa
  g <- 9.81
  rl <- 287.05
  p <- p0*exp(- (g*elev)/ (rl*t) )
  return(p)
}


pres_air_density <- function (...) {
  UseMethod("pres_air_density")
}

#' Air density
#'
#' Calculation of the air density.
#'
#' @param weather_station Object of class weather_station
#' @param height "lower" or "upper"
#'
#' @return Air density in kg/m^3.
#' @export
#'
pres_air_density.weather_station <- function(weather_station, height){
  if(height=="lower"){
    t <- weather_station$measurements$t1+273.15   # to Kelvin
    p <- weather_station$properties$p1
  } else if(height=="upper"){
    t <- weather_station$measurements$t2+273.15   # to Kelvin
    p <- weather_station$properties$p2
  }
  return(pres_air_density(p, t))
}


#' Air density
#'
#' Calculation of the air density.
#'
#' @param p Pressure in hPa.
#' @param t Temperature in degrees C.
#'
#' @return Air density in kg/m^3.
#' @export
#'
pres_air_density.numeric <- function(p, t){
  ad <- (p*100)/(287.05*(t+273.15))
  return(ad)
}
