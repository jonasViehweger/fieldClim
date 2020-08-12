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
pres_p_old <- function(elev, t){
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
pres_air_density_old <- function(p, t){
  ad <- (p*100)/(287.05*(t+273.15))
  return(ad)
}
