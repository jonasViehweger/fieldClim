#' Air pressure
#'
#' Calculation of pressure as a function of height.
#'
#' @param climate_station # *climate_station*-element.
#' @param height# # "lower" or "upper"
#'
#' @return Pressure in hPa.
#' @export
#'
#' @examples
pres_p <- function(climate_station, height){
  if(height=="lower"){
    t <- climate_station$climate_station_measurements$t1+273.15   # to Kelvin
    z <- climate_station$climate_station_location_properties$elevation + climate_station$climate_station_properties$z1
  } else if(height=="upper"){
    t <- climate_station$climate_station_measurements$t2+273.15   # to Kelvin
    z <- climate_station$climate_station_location_properties$elevation + climate_station$climate_station_properties$z2
  }
  p0 <- 1013.25    # Standardruck in hPa
  g <- 9.81
  rl <- 287.05
  p1 <- p0*exp(- (g*z)/ (rl*t1) )
  return(p1)
}

#' Air density
#'
#' Calculation of the air density.
#'
#' @param climate_station # *climate_station*-element.
#' @param height # "lower" or "upper"
#'
#' @return Air density in kg/m^3.
#' @export
#'
#' @examples
pres_air_density <- function(climate_station, height){
  if(height=="lower"){
    t <- climate_station$climate_station_measurements$t1+273.15   # to Kelvin
    p <- climate_station$climate_station_properties$p1
  } else if(height=="upper"){
    t <- climate_station$climate_station_measurements$t2+273.15   # to Kelvin
    p <- climate_station$climate_station_properties$p2
  }
  ad <- (p*100)/(287.05*t)
  return(ad)
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
pres_p <- function(elev, t){
  t <- t+273.15   # to Kelvin
  p0 <- 1013.25    # Standardruck in hPa
  g <- 9.81
  rl <- 287.05
  p <- p0*exp(- (g*elev)/ (rl*t) )
  return(p)
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
pres_air_density <- function(p, t){
  ad <- (p*100)/(287.05*(t+273.15))
  return(ad)
}
