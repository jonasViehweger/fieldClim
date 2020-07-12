#-----Funktion der relativen optischen Luftmasse; mit sh = Sonnenhoee in Grad
trans_air_mass_rel <- function(sh) {
  f <- pi/180                                 # Winkel in Radiant Faktor
  mr <- 1 / (sin(sh*f) + (1.5*sh**-0.72) )
  return(ifelse(sh <= 0, NA, mr))
}


#-------Funktion Absolute optische Luftmasse, mit Lufdruck p in hPa
trans_air_mass_abs <- function (air_mass_rel, p) {
  p0 <- 1013.25 # hPa Normaldruck auf Meeresniveau der Standardatmosph?re
  air_mass_abs <- air_mass_rel*(p/p0);
  return (air_mass_abs)
}

#------Funktion Transmission durch Rayleighstreuung, mit absoluter optischer Luftmasse
#' Transmittance due to rayleigh scattering
#'
#' @param air_mass_abs
#'
#' @return
#' @export
#'
#' @examples
trans_rayleigh <- function(air_mass_abs) {
  x <- (-0.0903)*air_mass_abs**0.84*(1.+air_mass_abs-air_mass_abs**1.01)
  return(exp(x))
}

#-------Funktion Transmission Ozon, mit Ozons?ule und relativer optischer Luftmasse
# oz: average Ozon columnar values 0.35 cm
trans_ozone <- function(air_mass_rel, oz) {
  x <- oz*air_mass_rel
  xx <- 0.1611*x*(1+139.48*x)**-0.3035-0.002715*x*(1+0.044*x+0.0003*x**2)**-1
  return(1.-xx)
}

#-------Funktion Transmission Wasserdampf, mit Wasserdampfs?ule (precipitable water) und relativer optischer Luftmasse
trans_vapor <- function(air_mass_rel, pw) {
  y <- pw*air_mass_rel
  yy <- 2.4959*y*((1+79.034*y)**0.6828+6.385*y)**-1
  return(1.-yy)
}


# -------Funktion Transmission Aerosol, mit Sichtweite in km und absoluter optischer Luftmasse
trans_aerosol <- function(air_mass_abs, vis) {
  tau38 <- 3.6536*vis**-0.7111
  tau5 <- 2.4087*vis**-0.719
  tex <- 0.2758*tau38+0.35*tau5
  x <- (tex**0.873*(-1))*(1.+tex-tex**0.7088)*air_mass_abs**0.9108
  return(exp(x))
}

#-------Funktion Transmission Gas (O2, CO2) mit absoluter optischer Luftmasse
trans_gas <- function(air_mass_abs) {
  return(exp(-0.0127*air_mass_abs**0.26))
}


#-------Funktion Gesamttransmission
# sh:  solar-height in degrees
# p:   pressure in hPa
# t:   temperature in C
# h:   altitude
# oz:  average Ozon columnar values 0.35 cm
# vis: Visibility in km (clear day = 30km)
trans_total <- function(solar_height, temp, altitude,
                        oz = 0.35, vis = 30, pressure = NULL){
  if(is.null(p)) p <- pp(h, temp)
  pw <- pw(p, temp, h)
  mr <- mr(sh)
  ma <- ma(mr, p)
  trans_total <- data.frame(rayleigh = trans_rayleigh(ma),
                            ozone = trans_ozone(mr, oz),
                            vapor = trans_vapor(mr, pw),
                            aerosol = trans_aerosol(ma, vis),
                            gas = trans_gas(ma))
  trans_total$total <- apply(trans_total, 1, prod)
  return(trans_total)
}
