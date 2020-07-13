#' Potential Temperature.
#' 
#' Calculation of the potential air temperature.
#'
#' @param p Pressure in hPa.
#' @param T1 Temperature in °C.
#'
#' @return 
#' @export
#'
#' @examples
temp_pot_temp <- function(T1, p){
  p0 <- 1013.25    # Standardruck in hPa
  air_const <- 0.286     # spezifische Gaskonstante / spezifische Wärmekapatität; Wert für Luft
  pot_temp <- T1*(p0/p)**air_const
  return(pot_temp)
}