#' Eccentricity
#'
#' @param datetime POSIXt object (POSIXct, POSIXlt)
#'
#' @return Eccentricity at the date
#' @export
#'
sol_eccentricity <- function(datetime) {

  if(!inherits(datetime, "POSIXt")){
    stop("datetime has to be of class POSIXt.")
  }

  # day of year
  doy <- strftime(datetime, format = "%j")

  x <- 2.*pi*(doy-1)/365.
  exz <- 1.00011+0.034221*cos(x)+0.00128*sin(x)+0.000719*cos(2*x)+0.000719*sin(2.*x)
  return(exz)
}

#' Solar azimuth and elevation angles
#'
#' Calculates solar azimuth and solar elevation angle.
#'
#' @param datetime POSIXt object (POSIXct, POSIXlt).
#' @param lat Latitude in decimal degrees.
#' @param lon Longitude in decimal degrees.
#'
#' @return data.frame with two columns: sol_azimuth and sol_elevation
#' @export
#'
sol_angles <- function(datetime, lat, lon){

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
  gbr <- lat*f
  glr <- lon*f
  #---------------------------Mittlere Sonnenzeit
  t <- ut+lon/15.           # In Stunden
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
  saz <- (sde*cos(gbr)-cos(asin(sde))*sin(gbr)*cos(h))/cos(shh)
  saz <- ifelse(t <=12, acos(saz), 360*f-acos(saz))
  saz_deg <- saz/f
  sh <- asin(shh)/f
  results <- data.frame(sol_azimuth = saz_deg,
                        sol_elevation = sh)
  return(results)
}

#' Solar elevation angle
#'
#' @param datetime POSIXt object (POSIXct, POSIXlt)
#' @param lat Latitude in decimal degrees.
#' @param lon Longitude in decimal degrees.
#'
#' @return Solar elevation angle in degrees.
#' @export
#'
sol_elevation <- function(datetime, lat, lon) {
  angles <- sol_angles(datetime, lat, lon)
  return(angles$sol_elevation)
}


#' Solar azimuth angle
#'
#' @param datetime POSIXt object (POSIXct, POSIXlt)
#' @param lat Latitude in decimal degrees.
#' @param lon Longitude in decimal degrees.
#'
#' @return Solar azimuth angle in degrees.
#' @export
#'
#' @examples
sol_azimuth <- function(datetime, lat, lon) {
  angles <- sol_angles(datetime, lat, lon)
  return(angles$sol_azimuth)
}
