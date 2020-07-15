#' Pressure.
#'
#' Calculation of pressure as a function of height.
#'
#' @param elev Elevation above sea level in m.
#' @param t1 Temperature in °C.
#'
#' @return Pressure in hPa.
#' @export
#'
#' @examples
pres_p <- function(elev,t1){
  t <- t1+273.15   # von ?c in K
  p0 <- 1013.25    # Standardruck in hPa
  g <- 9.81
  rl <- 287.05
  p <- p0*exp(- (g*z)/ (rl*t) );
  return(p)
}


#' Air density.
#'
#' Calculation of the air density.
#'
#' @param p Pressure in hPa.
#' @param t1 Temperature in °C.
#'
#' @return Air density in kg*m^(-3)
#' @export
#'
#' @examples
pres_air_density <- function(p, t1){
  ad <- p/(287.05*t1)
  return(ad)
}
