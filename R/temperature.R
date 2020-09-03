#' Potential Temperature
#'
#' Calculation of the potential air temperature.
#'
#' @rdname temp_pot_temp
#' @param ... Additional parameters passed to later functions.
#' @return Potential temperature in K.
#' @export
#'
temp_pot_temp <- function (...) {
  UseMethod("temp_pot_temp")
}

#' @rdname temp_pot_temp
#' @method temp_pot_temp numeric
#' @param p Pressure in hPa.
#' @param t Temperature in degrees C.
temp_pot_temp.numeric <- function(t, p, ...){
  p0 <- 1013.25    # Standardruck in hPa
  air_const <- 0.286     # spezifische Gaskonstante / spezifische Wärmekapatität; Wert für Luft
  pot_temp <- (t+273.15)*(p0/p)**air_const
  return(pot_temp)
}

#' @rdname temp_pot_temp
#' @method temp_pot_temp weather_station
#' @param weather_station Object of class weather_station
#' @param height Height of measurement, either "upper" or "lower".
temp_pot_temp.weather_station <- function(weather_station, height = "lower", ...){
  if(height=="lower"){
    check_availability(weather_station, "t1", "p1")
    t <- weather_station$measurements$t1   # to Kelvin
    p <- weather_station$properties$p1
  } else if(height=="upper"){
    check_availability(weather_station, "t2", "p2")
    t <- weather_station$measurements$t2   # to Kelvin
    p <- weather_station$properties$p2
  }
  check_availability(t, p)
  return(temp_pot_temp(t,p))
}
