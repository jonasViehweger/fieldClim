#' Potential Temperature
#'
#' Calculation of the potential air temperature.
#'
#' @param p Pressure in hPa.
#' @param t1 Temperature in degrees C.
#'
#' @return Potential temperature in K.
#' @export
#'
#' @examples
temp_pot_temp <- function(t, p){
  p0 <- 1013.25    # Standardruck in hPa
  air_const <- 0.286     # spezifische Gaskonstante / spezifische Wärmekapatität; Wert für Luft
  pot_temp <- (t+273.15)*(p0/p)**air_const
  return(pot_temp)
}
