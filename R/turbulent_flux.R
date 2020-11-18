#' Monin-Obhukov-Length
#'
#' Calculation of the Monin-Obhukov-Length.
#' Stability of atmosphere needs to be given as one of "stabil", "neutral" or "unstabil".
#'
#' @rdname turb_flux_monin
#' @param ... Additional parameters passed to later functions.
#' @return Monin-Obhukov-Length in m.
#' @export
#'
turb_flux_monin <- function (...) {
  UseMethod("turb_flux_monin")
}

#' @rdname turb_flux_monin
#' @method turb_flux_monin numeric
#' @param grad_rich_no Gradient-Richardson-Number.
#' @param z1 Lower height of measurement (e.g. height of anemometer) in m.
#' @param z2 Upper height of measurement in m.
#' @param z0 Roughness length in m.
#' @param v1 Windspeed at lower height (e.g. height of anemometer) in m/s.
#' @param v2 Windspeed at upper height in m/s.
#' @param t1 Temperature at lower height (e.g. height of anemometer) in degrees C.
#' @param t2 Temperature at upper height in degrees C.
#' @export
turb_flux_monin.numeric <- function(grad_rich_no, z1 = 2, z2 = 10, z0, v1, v2, t1, t2, ...){
  ustar <- turb_ustar(v1,z1,z0)
  monin <- rep(NA, length(grad_rich_no))
  for(i in 1:length(grad_rich_no)){
    if(is.na(grad_rich_no[i])){
      monin[i] <- NA
    } else if(ustar[i] < 0.2){
      monin[i] <- NA
    } else if(grad_rich_no[i] <= -0.05){
      monin[i] <- (z1*(t1[i]+273.15)*(((v2[i]-v1[i])/(z2-z1))**2))/(9.81*(t2[i]-t1[i])/(z2-z1))

    } else if(grad_rich_no[i] > -0.05 && grad_rich_no[i] < 0.05){
      monin[i] <- 0.75*(z1*(t1[i]+273.15)*(((v2[i]-v1[i])/(z2-z1))**2))/(9.81*(t2[i]-t1[i])/(z2-z1))

    } else if(grad_rich_no[i] >= 0.05){
      monin[i] <- 4.7*ustar[i]*log(z1/z0)*(z1-z0)/(v1[i]*0.4)
    }
  }

  if(any(is.na(monin))){
    warning("NAs were introduced, due to a either small friction velocity (ustar < 0.2), or missing Gradient-Richardson numbers.")
  }

  return(monin)
}

#' @rdname turb_flux_monin
#' @method turb_flux_monin weather_station
#' @param weather_station Object of class weather_station
#' @export
turb_flux_monin.weather_station <- function(weather_station, ...){
  check_availability(weather_station, "z1", "z2", "v1", "v2", "t1", "t2")
  grad_rich_no <- turb_flux_grad_rich_no(weather_station)
  z1 <- weather_station$properties$z1
  z2 <- weather_station$properties$z2
  z0 <- turb_roughness_length(weather_station)
  v1 <- weather_station$measurements$v1
  v2 <- weather_station$measurements$v2
  t1 <- weather_station$measurements$t1
  t2 <- weather_station$measurements$t2
  return(turb_flux_monin(grad_rich_no, z1, z2, z0, v1, v2, t1, t2))
}



#' Gradient-Richardson-Number
#'
#' Calculation of the Gradient-Richardson-Number. The number represents the
#' stability of the atmosphere. Negative values signify unstable conditions,
#' positive values signify stable conditions.
#'
#' @rdname turb_flux_grad_rich_no
#' @param ... Additional parameters passed to later functions.
#' @return Gradient-Richardson-Number.
#' @export
#'
turb_flux_grad_rich_no <- function (...) {
  UseMethod("turb_flux_grad_rich_no")
}

#' @rdname turb_flux_grad_rich_no
#' @method turb_flux_grad_rich_no numeric
#' @param t1 Temperature at lower height (e.g. height of anemometer) in degrees C.
#' @param t2 Temperature at upper height in degrees C.
#' @param z1 Lower height of measurement (e.g. height of anemometer) in m.
#' @param z2 Upper height of measurement in m.
#' @param v1 Windspeed at lower height (e.g. height of anemometer) in m/s.
#' @param v2 Windspeed at upper height in m/s.
#' @param p1 Pressure at lower height (e.g. height of anemometer) in hPa.
#' @param p2 Pressure at upper height in hPa.
#' @export
turb_flux_grad_rich_no.numeric <- function(t1, t2, z1 = 2, z2 = 10, v1, v2, p1, p2, ...){
  pot_temp1 <- temp_pot_temp(t1, p1)
  pot_temp2 <- temp_pot_temp(t2, p2)
  grad_rich_no <- (9.81/pot_temp1)*((pot_temp2-pot_temp1)/(z2-z1))*(((v2-v1)/(z2-z1))**(-2))
  grad_rich_no <- ifelse(is.nan(grad_rich_no), 0, grad_rich_no)
  return(grad_rich_no)
}

#' @rdname turb_flux_grad_rich_no
#' @method turb_flux_grad_rich_no weather_station
#' @param weather_station Object of class weather_station
#' @export
turb_flux_grad_rich_no.weather_station <- function(weather_station, ...){
  check_availability(weather_station, "z1", "z2", "v1", "v2", "t1", "t2", "p1", "p2")
  t1 <- weather_station$measurements$t1
  t2 <- weather_station$measurements$t2
  z1 <- weather_station$properties$z1
  z2 <- weather_station$properties$z2
  v1 <- weather_station$measurements$v1
  v2 <- weather_station$measurements$v2
  p1 <- weather_station$measurements$p1
  p2 <- weather_station$measurements$p2
  return(turb_flux_grad_rich_no(t1, t2, z1, z2, v1, v2, p1, p2))
}



#' Stability
#'
#' Conversion of Gradient-Richardson-Number to stability string.
#'
#' @rdname turb_flux_stability
#' @param ... Additional parameters passed to later functions.
#' @return Gradient-Richardson-Number.
#' @export
#'
turb_flux_stability <- function (...) {
  UseMethod("turb_flux_stability")
}

#' @rdname turb_flux_stability
#' @method turb_flux_stability numeric
#' @param grad_rich_no Gradient-Richardson-Number
#' @export
turb_flux_stability.numeric <- function(grad_rich_no, ...){
  stability <- rep(NA, length(grad_rich_no))
  for(i in 1:length(grad_rich_no)){
    if(is.na(grad_rich_no[i])){stability[i] <- NA}
    else if(grad_rich_no[i] <= -0.05){stability[i] <- "unstable"}
    else if(grad_rich_no[i] > -0.05 && grad_rich_no[i] < 0.05){stability[i] <- "neutral"}
    else if(grad_rich_no[i] >= 0.05){stability[i] <- "stable"}
  }
  return(stability)
}

#' @rdname turb_flux_stability
#' @method turb_flux_stability weather_station
#' @param weather_station Object of class weather_station
#' @export
turb_flux_stability.weather_station <- function(weather_station, ...){
  grad_rich_no <- turb_flux_grad_rich_no(weather_station)
  return(turb_flux_stability(grad_rich_no))
}


#' Exchange quotient for heat transmission
#'
#' Calculation of the exchange quotient of the turbulent heat transmission.
#'
#' @rdname turb_flux_ex_quotient_temp
#' @param ... Additional parameters passed to later functions.
#' @return Exchange quotient for heat transmission in kg/(m*s).
#' @export
turb_flux_ex_quotient_temp <- function (...) {
  UseMethod("turb_flux_ex_quotient_temp")
}

#' @rdname turb_flux_ex_quotient_temp
#' @method turb_flux_ex_quotient_temp numeric
#' @param grad_rich_no Gradient-Richardson-Number.
#' @param ustar Friction velocity in m/s.
#' @param monin Monin-Obhukov-Length in m.
#' @param z Height in m.
#' @param air_density Air density in kg/m^3.
#' @export
turb_flux_ex_quotient_temp.numeric <- function(grad_rich_no, ustar, monin, z, air_density, ...){
  ex <- rep(NA, length(grad_rich_no))
  for(i in 1:length(grad_rich_no)){
    if(is.na(grad_rich_no[i])){
      ex[i] <- NA
    } else if(grad_rich_no[i] <= -0.05){
      ex[i] <- (0.4*ustar[i]*z/(0.74*(1-9*z/monin[i])**(-0.5)))*air_density[i]
    } else if(grad_rich_no[i] > -0.05 && grad_rich_no[i] < 0.05){
      ex[i] <- (0.4*ustar[i]*z/(0.74+4.7*z/monin[i]))*air_density[i]
    } else if(grad_rich_no[i] >= 0.05){
      ex[i] <- NA
    }
  }
  return(ex)
}

#' @rdname turb_flux_ex_quotient_temp
#' @method turb_flux_ex_quotient_temp weather_station
#' @param weather_station Object of class weather_station
#' @param height Height of measurement. Either "upper" or "lower".
#' @export
turb_flux_ex_quotient_temp.weather_station <- function(weather_station, height = "lower", ...){
  grad_rich_no <- turb_flux_grad_rich_no(weather_station)
  ustar <- turb_ustar(weather_station)
  monin <- turb_flux_monin(weather_station)

  if(height == "lower"){
    check_availability(weather_station, "z1")
    z <- weather_station$properties$z1
  }
  if(height == "upper"){
    check_availability(weather_station, "z2")
    z <- weather_station$properties$z2
  }

  air_density <- pres_air_density(weather_station, height)
  return(grad_rich_no, ustar, monin, z, air_density)
}


#' Exchange quotient for impulse transmission
#'
#' Calculation of the exchange quotient of the turbulent impulse transmission.
#'
#' @rdname turb_flux_ex_quotient_imp
#' @param ... Additional parameters passed to later functions.
#' @return Exchange quotient for impulse transmission in kg/(m*s).
#' @export
#'
turb_flux_ex_quotient_imp <- function (...) {
  UseMethod("turb_flux_ex_quotient_imp")
}

#' @rdname turb_flux_ex_quotient_imp
#' @method turb_flux_ex_quotient_imp numeric
#' @param grad_rich_no Gradient-Richardson-Number.
#' @param ustar Friction velocity in m/s.
#' @param monin Monin-Obhukov-Length in m.
#' @param z Observation height in m.
#' @param air_density Air density in kg/m^3.
#' @export
turb_flux_ex_quotient_imp.numeric <- function(grad_rich_no, ustar, monin, z, air_density, ...){
  ex <- rep(NA, length(grad_rich_no))
  for(i in 1:length(grad_rich_no)){
    if(is.na(grad_rich_no[i])){
      ex[i] <- NA
    } else if(grad_rich_no[i] <= -0.05){
      ex[i] <- (0.4*ustar[i]*z/((1.15*z/monin[i])**(-0.25)))*air_density[i]
    } else if(grad_rich_no[i] > -0.05 && grad_rich_no[i] < 0.05){
      ex[i] <- (0.4*ustar[i]*monin[i]/4.7)*air_density[i]
    } else if(grad_rich_no[i] >= 0.05){
      ex[i] <- (0.4*ustar[i]*z)*air_density[i]
    }
  }
  return(ex)
}

#' @rdname turb_flux_ex_quotient_imp
#' @method turb_flux_ex_quotient_imp weather_station
#' @param weather_station Object of class weather_station
#' @param height Height of measurement. Either "upper" or "lower".
#' @export
turb_flux_ex_quotient_imp.weather_station <- function(weather_station, height = "lower", ...){
  grad_rich_no <- turb_flux_grad_rich_no(weather_station)
  ustar <- turb_ustar(weather_station)
  monin <- turb_flux_monin(weather_station)

  if(height == "lower"){
    check_availability(weather_station, "z1")
    z <- weather_station$properties$z1
  }
  if(height == "upper"){
    check_availability(weather_station, "z2")
    z <- weather_station$properties$z2
  }

  air_density <- pres_air_density(weather_station, height)
  return(grad_rich_no, ustar, monin, z, air_density)
}


#' Turbulent impulse exchange
#'
#' Calculation of the turbulent impulse exchange.
#'
#' @rdname turb_flux_imp_exchange
#' @param ... Additional parameters passed to later functions.
#' @return Turbulent impulse exchange in kg/(m*s^2).
#' @export
#'
turb_flux_imp_exchange <- function (...) {
  UseMethod("turb_flux_imp_exchange")
}

#' @rdname turb_flux_imp_exchange
#' @method turb_flux_imp_exchange numeric
#' @param ex_quotient Exchange quotient in kg/(m*s).
#' @param v1 Windspeed at lower height (e.g. height of anemometer) in m/s.
#' @param v2 Windspeed at upper height in m/s.
#' @param z1 Lower height of measurement (e.g. height of anemometer) in m.
#' @param z2 Upper height of measurement in m.
#' @export
turb_flux_imp_exchange.numeric <- function(ex_quotient, v1, v2, z1 = 2, z2 = 10, ...){
  ia <- ex_quotient*(v2-v1)/(z2-z1)
  return(ia)
}

#' @rdname turb_flux_imp_exchange
#' @method turb_flux_imp_exchange weather_station
#' @param weather_station Object of class weather_station
#' @param height Height of measurement. Either "upper" or "lower".
#' @export
turb_flux_imp_exchange.weather_station <- function(weather_station, height = "lower", ...){
  check_availability(weather_station, "z1", "z2", "v1", "v2")
  ex_quotient <- turb_flux_ex_quotient_imp(weather_station, height)
  v1 <- weather_station$measurements$v1
  v2 <- weather_station$measurements$v2
  z1 <- weather_station$properties$z1
  z2 <- weather_station$properties$z2
  return(turb_flux_imp_exchange(ex_quotient, v1, v2, z1, z2))
}


#' Sensible and latent heat fluxes
#'
#' Calculate sensible and latent heat fluxes, using the methods of Priestly-Taylor, Bowen,
#' Monin and Penman (only latent).
#'
#' @param weather_station Object of class weather_station
#'
#' @return Object of class weather_station
#' @export
turb_flux_calc <- function(weather_station){
  stability <- turb_flux_stability(weather_station)
  sensible_pt <- sensible_priestley_taylor(weather_station)
  latent_pt <- latent_priestley_taylor(weather_station)
  sensible_bow <- sensible_bowen(weather_station)
  latent_bow <- latent_bowen(weather_station)
  sensible_mon <- sensible_monin(weather_station)
  latent_mon <- latent_monin(weather_station)
  latent_pen <- latent_penman(weather_station)

  weather_station$measurements$stability <- stability
  weather_station$measurements$sensible_priestley_taylor <- sensible_pt
  weather_station$measurements$latent_priestley_taylor <- latent_pt
  weather_station$measurements$sensible_bowen <- sensible_bow
  weather_station$measurements$latent_bowen <- latent_bow
  weather_station$measurements$sensible_monin <- sensible_mon
  weather_station$measurements$latent_monin <- latent_mon
  weather_station$measurements$latent_penman <- latent_pen

  return(weather_station)
}
