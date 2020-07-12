#c----Funktion zur Berechnung des Exzentrizit?tsfaktor
#' Eccentricity
#'
#' @param datetime
#'
#' @return
#' @export
#'
#' @examples
sol_eccentricity <- function(datetime) {

  if(!inherits(datetime, "POSIXt")){
    stop("datetime has to be of class POSIXt.")
  }

  # day of year
  doy <- strftime(datetime, format = "%j")

  x <- 2.*pi*(doy-1)/365.
  exz <- 1.00011+0.034221*cos(x)+0.00128*sin(x)+0.000719*cos(2*x)+0.000719*sin(2.*x);
  return(exz)
}

#' Solar elevation angle
#'
#' @param datetime
#' @param latitude
#' @param longitude
#'
#' @return
#' @export
#'
#' @examples
sol_elevation <- function(datetime, latitude, longitude) {

  if(!inherits(datetime, "POSIXt")){
    stop("datetime has to be of class POSIXt.")
  }

  # day of year
  doy <- strftime(datetime, format = "%j")
  # decimal hour
  lt <- as.POSIXlt(datetime)
  ut <- lt$hour + lt$min/60 + lt$sec/3600

  f <- pi/180   # Winkel in Radiant Faktor
  #---------------------------Anweisungsteil
  gbr <- latitude*f
  glr <- longitude*f
  #---------------------------Mittlere Sonnenzeit
  t <- ut+longitude/15.           # In Stunden
  #---------------------------Stundenwinkel in Radiant
  m <- 356.6+0.9856*doy      # in Grad
  m <- m*f                 # in Radiant
  zt <- 0.1644*sin(2.*glr) - 0.1277*sin(m) # in Stunden
  h <- (15.*f)*(t+zt-12.)  # In Radiant
  #---------------------------Scheinb. geoz etc.
  del <- 279.3*f+0.9856*f*doy+1.92*f*sin(356.6*f+0.9856*f*doy) #  in Radiant
  #---------------------------Sinus Deklination der Sonne
  sde <- sin(23.44*f)*sin(del)     #  In Radiant
  #---------------------------Sonnenh?he
  shh <- sin(gbr)*sde+cos(gbr)*cos(asin(sde))*cos(h)
  sh <- asin(shh)/f;
  return(sh)
}


#' Solar azimuth angle
#'
#' @param datetime
#' @param latitude
#' @param longitude
#'
#' @return
#' @export
#'
#' @examples
sol_azimuth <- function(datetime, latitude, longitude) {

  if(!inherits(datetime, "POSIXt")){
    stop("datetime has to be of class POSIXt.")
  }

  # day of year
  doy <- strftime(datetime, format = "%j")
  # decimal hour
  lt <- as.POSIXlt(datetime)
  ut <- lt$hour + lt$min/60 + lt$sec/3600

  f <- pi/180   # Winkel in Radiant Faktor
  #---------------------------Anweisungsteil
  gbr <- latitude*f
  glr <- longitude*f
  #---------------------------Mittlere Sonnenzeit
  t <- ut+longitude/15.           # In Stunden
  #---------------------------Stundenwinkel in Radiant
  m <- 356.6+0.9856*doy      # in Grad
  m <- m*f                 # in Radiant
  zt <- 0.1644*sin(2.*glr) - 0.1277*sin(m) # in Stunden
  h <- (15.*f)*(t+zt-12.)  # In Radiant
  #---------------------------Scheinb. geoz etc.
  del <- 279.3*f+0.9856*f*doy+1.92*f*sin(356.6*f+0.9856*f*doy) #  in Radiant
  #---------------------------Sinus Deklination der Sonne
  sde <- sin(23.44*f)*sin(del)     #  In Radiant
  #---------------------------Sonnenh?he
  shh <- sin(gbr)*sde+cos(gbr)*cos(asin(sde))*cos(h)
  #---------------------------Sonnenazimut
  if(t <= 12) {
    saz <- (sde*cos(gbr)-cos(asin(sde))*sin(gbr)*cos(h))/cos(shh)
    saz <- acos(saz)
  } else {
    saz <- (sde*cos(gbr)-cos(asin(sde))*sin(gbr)*cos(h))/cos(shh)
    saz <- 360.*f-acos(saz) }
  saz <- saz/f;
  return(saz)
}
