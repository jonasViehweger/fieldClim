#' Monin-Obhukov-Length.
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
#' @param T1 Temperature at lower height (e.g. height of anemometer) in °C.
#' @param T2 Temperature at upper height in °C.
#' @param ustar Friction velocity in m/s.
#'
#' @return Monin-Obhukov-Length in m.
#' @export
#'
#' @examples
turb_flux_mol <- function(stability, z1, z2, z0 = NULL, v1, v2, T1, T2, ustar){
  if(stability == "labil"){
    mol <- (z1*(T1+273-15)*(((v2-v1)/(z2-z1))**2))/(9.81*(T2-T1)/(z2-z1))
  }
  else if(stability == "neutral"){
    mol <- 0.75*(z1*(T1+273-15)*(((v2-v1)/(z2-z1))**2))/(9.81*(T2-T1)/(z2-z1))
  }
  else if(stability == "stabil"){
    mol <- 4.7*ustar*log(z1/z_0)*(z1-z_0)/(v1*0.4)
  }
  return(mol)
}

#' Gradient-Richardson-Number.
#'
#' Calculation of the Gradient-Richardson-Number.
#'
#' @param T1 Temperature at lower height (e.g. height of anemometer) in °C.
#' @param T2 Temperature at upper height in °C.
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
turb_flux_grad_rich_no <- function(T1, T2, z1, z2, v1, v2, p1, p2){
  pot_temp1 <- temp_pot_temp(T1, p1)
  pot_temp2 <- temp_pot_temp(T2, p1)
  grad_rich_no <- round((9.81/pot_temp1)*((pot_temp2-pot_temp1)/(z2-z1))*(((v2-v1)/(z2-z1))**(-2)), 2)
  return(grad_rich_no)}

#' Stability.
#'
#' Calculation of atmospheric stability.
#'
#' @param grad_rich_no Gradient-Richardson-Number
#'
#' @return Stability of atmosphere. Returns one of "stabil", "neutral" or "instabil".
#' @export
#'
#' @examples
turb_flux_stability <- function(grad_rich_no){
  if(grad_rich_no < 0){stability <- "labil"}
  else if(grad_rich_no == 0){stability <- "neutral"}
  else if(grad_rich_no > 0){stability <- "stabil"}
  return(stability)
}

#' Exchange quotient for heat transmission.
#'
#' Calculation of the exchange quotient of the turbulent heat transmission.
#'
#' @param stability Stability of atmosphere. One of "stabil", "neutral" or "instabil".
#' @param ustar Friction velocity in m/s.
#' @param mol Monin-Obhukov-Length in m.
#' @param z1 Height in m.
#' @param air_density Air density in kg*m^(-3).
#'
#' @return Exchange quotient for heat transmission in kg/(m*s).
#' @export
#'
#' @examples
turb_flux_ex_quotient_temp <- function(stability, ustar, mol, z1, air_density){
  if(stability == "labil"){
    k <- 0.4*ustar*z1/(0.74*(1-9*z1/mol)**(-0.5))
    A <- k*air_density
  }
  if(stability == "stabil"){
    k <- 0.4*ustar*z1/(0.74+4.7*z1/mol)
    A <- k*air_density
  }
  if(stability == "neutral"){
    A <- NA
  }
  return(A)
}

#' Exchange quotient for impulse transmission.
#'
#' Calculation of the exchange quotient of the turbulent impulse transmission.
#'
#' @param stability Stability of atmosphere. One of "stabil", "neutral" or "instabil".
#' @param ustar Friction velocity in m/s.
#' @param mol Monin-Obhukov-Length in m.
#' @param z1 Height in m.
#' @param air_density Air density in kg*m^(-3).
#'
#' @return Exchange quotient for impulse transmission in kg/(m*s).
#' @export
#'
#' @examples
turb_flux_ex_quotient_imp <- function(stability, ustar, mol, z1, air_density){
  if(stability == "labil"){
    k <- 0.4*ustar*z1/((1-15*z1/mol)**(-0.25))
  }
  if(stability == "stabil"){
    k <- 0.4*ustar*mol/4.7
  }
  if(stability == "neutral"){
    k <- 0.4*ustar*z1
  }
  A <- k*air_density
  return(A)
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
turb_flux_imp_exchange <- function(ex_quotient, v1, v2, z1, z2){
  ia <- ex_quotient*(v2-v1)/(z2-z1)
  return(ia)
}
