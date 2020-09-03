#' Mechanical internal boundary layer; lowest height.
#'
#' Calculation of the lowest height of the mechanical internal boundary layer.
#'
#' @param dist Distance to point of roughness change in m.
#'
#' @return Height of boundary layer in m.
#' @export
#'
bound_mech_low <- function(dist) {
  mib <- 0.3*sqrt(dist)
  return(mib)
}

#' Mechanical internal boundary layer; average height.
#'
#' Calculation of the average height of the mechanical internal boundary layer.
#'
#' @param dist Distance to point of roughness change in m.
#'
#' @return Height of boundary layer in m.
#' @export
#'
bound_mech_avg <- function(dist) {
  mib <- 0.43*dist**0.5
  return(mib)
}

#' Thermal internal boundary layer.
#'
#' Calculation of the average height of the thermal internal boundary layer.
#'
#' @param ustar Friction velocity u* in m/s.
#' @param v Windspeed in height of anemometer in m/s.
#' @param temp_change_dist Distance to point of temperature change in m.
#' @param t_pot_upwind Potential temperature in upwind direction in °C.
#' @param t_pot Potential temperature at site in °C.
#' @param lapse_rate Lapse rate in K/m (or degrees C/m)
#'
#' @return Average height of the thermal boundary layer in m.
#' @export
#'
bound_thermal_avg <- function(ustar, v, temp_change_dist, t_pot_upwind, t_pot, lapse_rate) {
  tib <- (ustar/v)*( (temp_change_dist*abs(t_pot_upwind-t_pot))/abs(lapse_rate) )**0.5
  return(tib)
}
