#' Sensible Heat Priestley-Taylor Method
#'
#' Calculates the Sensible heat flux using the Priestley-Taylor method. Negative
#' heat flux signifies flux away from the surface, positive values signify flux
#' towards the surface.
#'
#' @param t1 Air temperature in degrees celsius
#' @param rad_bal Radiation balance (W/m^2)
#' @param soil_flux Soil flux (W/m^2)
#'
#' @return Sensible heat flux (W/m^2)
#' @export
#'
sensible_priestley_taylor <- function(t1, rad_bal, soil_flux){
  sc <- sc(t1)
  lamb <- lamb(t1)
  alpt <- 1.25
  QH_TP <- ((1-alpt)*sc+lamb)*(-rad_bal-soil_flux)/(sc+lamb)
  return(QH_TP)
}


#' Sensible Heat using Monin-Obukhov length
#'
#' Calculates the sensible heat flux using the Monin-Obukhov length. Negative
#' flux signifies flux away from the surface, positive values signify flux
#' towards the surface.
#'
#' @param air_density Air density in (kg*m^-3)
#' @param monin Monin-Obukhov length (m)
#' @param t_gradient Specific moisture gradient (dq/dlog(z) with q in kg/kg)
#' @param ustar Friction velocity (m/s)
#' @param ri Gradient-Richardson-Number
#' @param z1 Height of air temperature measurment (m)
#'
#' @return Sensible heat flux (W/m^2)
#' @export
#'
sensible_monin <- function(air_density, monin, t_gradient, ustar, ri, z1) {
  cp <- 1004.834
  k <- 0.4
  s1 <- z1/monin
  busi[ri<=0] <- 0.95*(1-(11.6*s1))^-0.5
  busi[ri>0] <- 0.95+(7.8*s1)
  QH <- -1*((air_density*cp*k*ustar)/busi)*1*t_gradient
  return(QH)
}


#' Sensible Heat using Bowen Method
#'
#' Calculates the sensible heat flux using the Bowen Method. Negative
#' flux signifies flux away from the surface, positive values signify flux
#' towards the surface.
#'
#' @param t1 Temperature at lower height (e.g. height of anemometer) in degrees C.
#' @param t2 Temperature at upper height in degrees C.
#' @param h1 Relative humidity at lower height (e.g. height of anemometer) in %.
#' @param h2 Relative humidity at upper height in %.
#' @param z1 Lower height of measurement (e.g. height of anemometer) in m.
#' @param z2 Upper height of measurement in m.
#' @param rad_bal Radiation balance in W/m^2
#' @param soil_flux Soil flux in W/m^2
#' @param p OPTIONAL. Air pressure in hPa. If not given, elev has to be set.
#' @param elev OPTIONAL. Elevation above sea level in m. If not given, p has to be set.
#'
#' @return Sensible heat flux (W/m^2)
#' @export
#'
sensible_bowen <- function(t1, t2, h1, h2, z1, z2,
                      rad_bal, soil_flux,
                      p = NULL, elev = NULL){

  # Calculating pressure
  if(is.null(p) & is.null(elev)) stop("Either 'p' or 'elev' have to be set.")
  if(is.null(p)) p <- pres_p(altitude, temp)

  # Calculating potential temperature delta
  t1_pot <- temp_pot_temp(t1, p)
  t2_pot <- temp_pot_temp(t2, p)
  dpot <- (t1_pot-t2_pot) / (z1-z2)

  # Calculating absolute humidity
  af1 <- hum_absolute(hum_vapor_pres(h1, svp(t1)), t1_pot)
  af2 <- hum_absolute(hum_vapor_pres(h2, svp(t2)), t2_pot)
  dah <- (af1-af2) / (z1-z2)

  # calculate bowen ratio
  bowen_ratio <- bowen_ratio(t1_pot, dpot, dah)
  return(-rad_bal-soil_flux) * (bowen_ratio / (1+bowen_ratio))
}
