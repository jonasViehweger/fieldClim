#' Relative optical air mass
#'
#' Calculates relative optical air mass.
#'
#' @param sol_elevation Solar elevation in degrees.
#'
#' @return Relative optical air mass.
#' @export
#'
trans_air_mass_rel <- function(sol_elevation) {
  f <- pi/180                                 # Winkel in Radiant Faktor
  mr <- 1 / (sin(sh*f) + (1.5*sh**-0.72) )
  return(ifelse(sh <= 0, NA, mr))
}


#' Absolute optical air mass
#'
#' Calculates absolute optical air mass.
#'
#' @param air_mass_rel Relative optical air mass.
#' @param pressure Pressure in hPa.
#'
#' @return Absolute optical air mass.
#' @export
#'
#' @examples
trans_air_mass_abs <- function (air_mass_rel, pressure) {
  p0 <- 1013.25 # hPa Normaldruck auf Meeresniveau der Standardatmosph?re
  air_mass_abs <- air_mass_rel*(p/p0);
  return (air_mass_abs)
}


#' Transmittance due to rayleigh scattering
#'
#' Calculates transmittance due to rayleigh scattering
#'
#' @param air_mass_abs Absolute optical air mass.
#'
#' @return Transmittance due to rayleigh scattering (0-1)
#' @export
#'
#' @examples
trans_rayleigh <- function(air_mass_abs) {
  x <- (-0.0903)*air_mass_abs**0.84*(1.+air_mass_abs-air_mass_abs**1.01)
  return(exp(x))
}

#' Transmittance due to ozone
#'
#' Calculates transmittance due to ozone
#'
#' @param air_mass_rel Relative optical air mass.
#' @param oz Columnar ozone in cm. Default is average value of 0.35 cm
#'
#' @return Transmittance due to ozone (0-1)
#' @export
#'
#' @examples
trans_ozone <- function(air_mass_rel, oz = 0.35) {
  x <- oz*air_mass_rel
  xx <- 0.1611*x*(1+139.48*x)**-0.3035-0.002715*x*(1+0.044*x+0.0003*x**2)**-1
  return(1.-xx)
}


#' Transmittance due to water vapor
#'
#' Calculates transmittance due to water vapor
#'
#' @param air_mass_rel Relative optical air mass.
#' @param pw Precipitable water in cm.
#'
#' @return Transmittance due to water vapor (0-1)
#' @export
#'
#' @examples
trans_vapor <- function(air_mass_rel, pw) {
  y <- pw*air_mass_rel
  yy <- 2.4959*y*((1+79.034*y)**0.6828+6.385*y)**-1
  return(1.-yy)
}


#' Transmittance due to aerosols
#'
#' @param air_mass_abs Absolute optical air mass.
#' @param vis Meteorological visibility in km. Default is the visibility on a clear day.
#'
#' @return Transmittance due to aerosols (0-1)
#' @export
#'
#' @examples
trans_aerosol <- function(air_mass_abs, vis = 30) {
  tau38 <- 3.6536*vis**-0.7111
  tau5 <- 2.4087*vis**-0.719
  tex <- 0.2758*tau38+0.35*tau5
  x <- (tex**0.873*(-1))*(1.+tex-tex**0.7088)*air_mass_abs**0.9108
  return(exp(x))
}

#' Transmittance due to gas
#'
#' Calculates transmittance due to O2 and CO2.
#'
#' @param air_mass_abs Absolute optical air mass.
#'
#' @return Transmittance due to gas (0-1)
#' @export
#'
#' @examples
trans_gas <- function(air_mass_abs) {
  return(exp(-0.0127*air_mass_abs**0.26))
}


#' Total transmittance
#'
#' Calculates total transmittance of the atmosphere.
#'
#' @param sol_elevation Solar elevation in degrees.
#' @param t1 Air temperature in degrees celsius.
#' @param elev Altitude above sea level in m.
#' @param oz Columnar ozone in cm. Default is
#' @param vis Meteorological visibility in km. Default is the visibility on a clear day.
#' @param pressure OPTIONAL. Pressure in hPa. Calculated from elev and t1 if not available.
#'
#' @return Total transmittance (0-1)
#' @export
#'
#' @examples
trans_total <- function(sol_elevation, t1, elev,
                        oz = 0.35, vis = 30, pressure = NULL){
  if(is.null(p)) p <- pres_p(elev, t1)
  pw <- pw(p, t1, elev)
  mr <- trans_air_mass_rel(sol_elevation)
  ma <- trans_air_mass_abs(mr, pressure)
  trans_total <- data.frame(rayleigh = trans_rayleigh(ma),
                            ozone = trans_ozone(mr, oz),
                            vapor = trans_vapor(mr, pw),
                            aerosol = trans_aerosol(ma, vis),
                            gas = trans_gas(ma))
  trans_total$total <- apply(trans_total, 1, prod)
  return(trans_total)
}
