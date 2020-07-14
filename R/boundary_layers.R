#' Mechanical internal boundary layer; lowest height.
#'
#' Calculation of the lowest height of the mechanical internal boundary layer.
#'
#' @param dist Distance to point of roughness change in m.
#'
#' @return Height of boundary layer in m.
#' @export
#'
#' @examples
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
#' @examples
bound_mech_avg <- function(dist) {
  mib <- 0.43*dist**0.5
  return(mib)
}

#' Thermal internal boundary layer.
#'
#' Calculation of the average height of the thermal internal boundary layer.
#'
#' @param ustar Friction velocity u*.
#' @param v_a Windspeed in height of anemometer in m/s.
#' @param temp_change_dist Distance to point of temperature change in m.
#' @param pt_upwind Potential temperature in upwind direction in °C.
#' @param pt Potential temperature at site in °C.
#' @param lr Lapse rate in K/m (or °C/m)
#'
#' @return
#' @export
#'
#' @examples
bound_thermal_avg <- function(ustar,v_a,temp_change_dist,pt_upwind,pt,lr) {
  tib <- (ustar/v_a)*( (temp_change_dist*abs(pt_upwind-pt))/abs(lr) )**0.5;
  return(tib)
}
