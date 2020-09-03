#' Eccentricity
#'
#' Calculates the eccentricity.
#'
#' @param ... Additional parameters passed to later functions.
#' @return Eccentricity at the date.
#' @export
#'
sol_eccentricity <- function (...) {
  UseMethod("sol_eccentricity")
}

#' @rdname sol_eccentricity
#' @method sol_eccentricity POSIXt
#' @param datetime POSIXt object (POSIXct, POSIXlt).
#' See [base::as.POSIXlt] and [base::strptime] for conversion.
#' @export
sol_eccentricity.POSIXt <- function(datetime, ...) {

  if(!inherits(datetime, "POSIXt")){
    stop("datetime has to be of class POSIXt.")
  }

  # day of year
  doy <- as.numeric(strftime(datetime, format = "%j"))

  x <- 2.*pi*(doy-1)/365.
  exz <- 1.00011+0.034221*cos(x)+0.00128*sin(x)+0.000719*cos(2*x)+0.000719*sin(2.*x)
  return(exz)
}

#' @rdname sol_eccentricity
#' @method sol_eccentricity weather_station
#' @param weather_station Object of class weather_station.
#' @export
sol_eccentricity.weather_station <- function(weather_station, ...) {
  check_availability(weather_station, "datetime")

  datetime <- weather_station$measurements$datetime

  return(sol_eccentricity(datetime))
}



#' Solar azimuth and elevation angles
#'
#' Calculates solar azimuth and solar elevation angle.
#'
#' @param ... Additional parameters passed to later functions.
#' @return data.frame with two columns: sol_azimuth and sol_elevation.
#' @export
#'
sol_angles <- function(...) {
  UseMethod("sol_angles")
}

#' @rdname sol_angles
#' @method sol_angles POSIXt
#' @param datetime POSIXt object (POSIXct, POSIXlt).
#' See [base::as.POSIXlt] and [base::strptime] for conversion.
#' @param lat Latitude in decimal degrees.
#' @param lon Longitude in decimal degrees.
#' @export
sol_angles.POSIXt <- function(datetime, lat, lon, ...){

  if(!inherits(datetime, "POSIXt")){
    stop("datetime has to be of class POSIXt.")
  }

  # day of year
  doy <- as.numeric(strftime(datetime, format = "%j"))
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
  sh <- asin(shh)/f
  #---------------------------Sonnenazimut
  saz <- (sde*cos(gbr)-cos(asin(sde))*sin(gbr)*cos(h))/cos((sh*f))

  saz_2 <- rep(NA, length(saz))
  for(i in 1:length(saz_2)){
    if(t[i]<=12){saz_2[i] <- acos(saz[i])}
    if(t[i]>12){saz_2[i] <- 360*f-acos(saz[i])}
  }
  saz_deg <- saz_2/f
  results <- data.frame(sol_azimuth = saz_deg,
                        sol_elevation = sh)
  return(results)
}

#' @rdname sol_angles
#' @method sol_angles weather_station
#' @param weather_station Object of class weather_station.
#' @export
sol_angles.weather_station <- function(weather_station, ...) {
  check_availability(weather_station, "datetime", "latitude", "longitude")

  datetime <- weather_station$measurements$datetime
  lat <- weather_station$location_properties$latitude
  lon <- weather_station$location_properties$longitude

  return(sol_angles(datetime, lat, lon))
}



#' Solar elevation angle
#'
#' Calculates solar elevation angle for the given date and time.
#'
#' @param ... Additional parameters passed to later functions.
#' @return Solar elevation angle in degrees.
#' @export
#'
sol_elevation <- function (...) {
  UseMethod("sol_elevation")
}

#' @rdname sol_elevation
#' @method sol_elevation POSIXt
#' @param datetime POSIXt object (POSIXct, POSIXlt).
#' See [base::as.POSIXlt] and [base::strptime] for conversion.
#' @param lat Latitude in decimal degrees.
#' @param lon Longitude in decimal degrees.
#' @export
sol_elevation.POSIXt <- function(datetime, lat, lon, ...) {
  angles <- sol_angles(datetime, lat, lon)
  return(angles$sol_elevation)
}

#' @rdname sol_elevation
#' @method sol_elevation weather_station
#' @param weather_station Object of class weather_station.
#' @export
sol_elevation.weather_station <- function(weather_station, ...) {
  check_availability(weather_station, "datetime", "latitude", "longitude")

  datetime <- weather_station$measurements$datetime
  lat <- weather_station$location_properties$latitude
  lon <- weather_station$location_properties$longitude

  return(sol_elevation(datetime, lat, lon))
}



#' Solar azimuth angle
#'
#' Calculates solar azimuth angle for the given date and time.
#'
#' @param ... Additional parameters passed to later functions.
#' @return Solar azimuth angle in degrees.
#' @export
#'
sol_azimuth <- function (...) {
  UseMethod("sol_azimuth")
}

#' @rdname sol_azimuth
#' @method sol_azimuth POSIXt
#' @param datetime POSIXt object (POSIXct, POSIXlt).
#' See [base::as.POSIXlt] and [base::strptime] for conversion.
#' @param lat Latitude in decimal degrees.
#' @param lon Longitude in decimal degrees.
#' @export
sol_azimuth.POSIXt <- function(datetime, lat, lon, ...) {
  angles <- sol_angles(datetime, lat, lon)
  return(angles$sol_azimuth)
}

#' @rdname sol_azimuth
#' @method sol_azimuth weather_station
#' @param weather_station Object of class weather_station.
#' @export
sol_azimuth.weather_station <- function(weather_station, ...) {
  check_availability(weather_station, "datetime", "latitude", "longitude")

  datetime <- weather_station$measurements$datetime
  lat <- weather_station$location_properties$latitude
  lon <- weather_station$location_properties$longitude

  return(sol_azimuth(datetime, lat, lon))
}
