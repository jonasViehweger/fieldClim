#' Monin-Obhukov-Length
#'
#' Calculation of the Monin-Obhukov-Length.
#' Stability of atmosphere needs to be given as one of "stabil", "neutral" or "unstabil".
#'
#' @param stability Stability of atmosphere.
#' @param z1 Lower height of measurement (e.g. height of anemometer) in m.
#' @param z2 Upper height of measurement in m.
#' @param z0 Roughness length in m.
#' @param v1 Windspeed at lower height (e.g. height of anemometer) in m/s.
#' @param v2 Windspeed at upper height in m/s.
#' @param t1 Temperature at lower height (e.g. height of anemometer) in °C.
#' @param t2 Temperature at upper height in °C.
#' @param ustar Friction velocity in m/s.
#'
#' @return Monin-Obhukov-Length in m.
#' @export
#'
turb_flux_monin <- function(stability, z1 = 2, z2 = 10, z0, v1, v2, t1, t2){
  ustar <- turb_ustar(v1,z1,z0)
  monin[stability == "labil"] <- (z1*(t1+273-15)*(((v2-v1)/(z2-z1))**2))/(9.81*(t2-t1)/(z2-z1))
  monin[stability == "neutral"] <- 0.75*(z1*(t1+273-15)*(((v2-v1)/(z2-z1))**2))/(9.81*(t2-t1)/(z2-z1))
  monin[stability == "stabil"] <- 4.7*ustar*log(z1/z0)*(z1-z0)/(v1*0.4)
  return(monin)
}

#' Gradient-Richardson-Number
#'
#' Calculation of the Gradient-Richardson-Number. The number represents the
#' stability of the atmosphere. Negative values signify unstable conditions,
#' positive values signify stable conditions.
#'
#' @param t1 Temperature at lower height (e.g. height of anemometer) in °C.
#' @param t2 Temperature at upper height in °C.
#' @param z1 Lower height of measurement (e.g. height of anemometer) in m.
#' @param z2 Upper height of measurement in m.
#' @param v1 Windspeed at lower height (e.g. height of anemometer) in m/s.
#' @param v2 Windspeed at upper height in m/s.
#' @param p1 Pressure at lower height (e.g. height of anemometer) in hPa.
#' @param p2 Pressure at upper height in hPa.
#'
#' @return Gradient-Richardson-Number.
#' @export
#'
#' @examples
turb_flux_grad_rich_no <- function(t1, t2, z1 = 2, z2 = 10, v1, v2, p1, p2){
  pot_temp1 <- temp_pot_temp(t1, p1)
  pot_temp2 <- temp_pot_temp(t2, p1)
  grad_rich_no <- round((9.81/pot_temp1)*((pot_temp2-pot_temp1)/(z2-z1))*(((v2-v1)/(z2-z1))**(-2)), 2)
  return(grad_rich_no)}

#' Stability
#'
#' Calculation of atmospheric stability.
#'
#' @param grad_rich_no Gradient-Richardson-Number
#'
#' @return Stability of atmosphere. Returns one of "stabil", "neutral" or "instabil".
#' @export
#'
turb_flux_stability <- function(grad_rich_no){
  stability[grad_rich_no < 0] <- "labil"
  stability[grad_rich_no == 0] <- "neutral"
  stability[grad_rich_no > 0] <- "stabil"
  return(stability)
}

#' Exchange quotient for heat transmission
#'
#' Calculation of the exchange quotient of the turbulent heat transmission.
#'
#' @param stability Stability of atmosphere. One of "stabil", "neutral" or "instabil".
#' @param ustar Friction velocity in m/s.
#' @param monin Monin-Obhukov-Length in m.
#' @param z1 Height in m.
#' @param air_density Air density in kg/m^3.
#'
#' @return Exchange quotient for heat transmission in kg/(m*s).
#' @export
#'
turb_flux_ex_quotient_temp <- function(stability, ustar, monin, z1, air_density){
  ex[stability == "labil"] <- (0.4*ustar*z1/(0.74*(1-9*z1/monin)**(-0.5)))*air_density
  ex[stability == "stabil"] <- (0.4*ustar*z1/(0.74+4.7*z1/monin))*air_density
  ex[stability == "neutral"] <- NA
  return(ex)
}

#' Exchange quotient for impulse transmission
#'
#' Calculation of the exchange quotient of the turbulent impulse transmission.
#'
#' @param stability Stability of atmosphere. One of "stabil", "neutral" or "instabil".
#' @param ustar Friction velocity in m/s.
#' @param monin Monin-Obhukov-Length in m.
#' @param z1 Height in m.
#' @param air_density Air density in kg/m^3.
#'
#' @return Exchange quotient for impulse transmission in kg/(m*s).
#' @export
#'
turb_flux_ex_quotient_imp <- function(stability, ustar, monin, z1, air_density){
  ex[stability == "labil"] <- (0.4*ustar*z1/((1-15*z1/monin)**(-0.25)))*air_density
  ex[stability == "stabil"] <- (0.4*ustar*monin/4.7)*air_density
  ex[stability == "neutral"] <- (0.4*ustar*z1)*air_density
  return(ex)
}

#' Turbulent impulse exchange.
#'
#' Calculation of the turbulent impulse exchange.
#'
#' @param ex_quotient Exchange quotient in kg/(m*s).
#' @param v1 Windspeed at lower height (e.g. height of anemometer) in m/s.
#' @param v2 Windspeed at upper height in m/s.
#' @param z1 Lower height of measurement (e.g. height of anemometer) in m.
#' @param z2 Upper height of measurement in m.
#'
#' @return Turbulent impulse exchange in kg/(m*s^2).
#' @export
#'
#' @examples
turb_flux_imp_exchange <- function(ex_quotient, v1, v2, z1 = 2, z2 = 10){
  ia <- ex_quotient*(v2-v1)/(z2-z1)
  return(ia)
}
