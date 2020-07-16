#' Latent Heat Priestley-Taylor Method
#'
#' Calculates the latent heat flux using the Priestley-Taylor method. Negative
#' heat flux signifies flux away from the surface, positive values signify flux
#' towards the surface.
#'
#' @param t1 Air temperature in degrees Celsius.
#' @param rad_bal Radiation balance in W/m^2.
#' @param soil_flux Soil flux in W/m^2.
#'
#' @return Latent heat flux in W/m^2.
#' @export
#'
latent_priestley_taylor <- function(t1, rad_bal, soil_flux){
  sc <- sc(t1)
  lamb <- lamb(t1)
  alpt <- 1.25
  QE_TP <- alpt*sc*((-rad_bal-soil_flux)/sc+lamb) ;
  return(QE_TP)
}


#' Latent Heat Penman Method
#'
#' Calculates the latent heat flux using the Penman-Monteith equation. Negative
#' heat flux signifies flux away from the surface, positive values signify flux
#' towards the surface.
#'
#' @param datetime POSIXt object (POSIXct, POSIXlt).
#' @param v1 Wind velocity in m/s.
#' @param t1 Temperature in degrees C
#' @param h1 Relative humidity in percent (0-100)
#' @param z1 Height of measurement for t1, v1 in m.
#' @param rad_bal Radiation balance in W/m^2.
#' @param elevation Elevation above sea level in m.
#' @param lat Latitude in decimal degrees.
#' @param lon Longitude in decimal degrees.
#'
#' @return Latent heat flux in W/m^2.
#' @import water
#' @export
#'
latent_penman <- function(datetime,
                       v1, t1, h1, z1, rad_bal,
                       elevation, lat, lon){
  if(!inherits(datetime, "POSIXt")){
    stop("datetime has to be of class POSIXt.")
  }

  # day of year
  doy <- strftime(datetime, format = "%j")
  # decimal hour
  lt <- as.POSIXlt(datetime)
  ut <- lt$hour + lt$min/60 + lt$sec/3600

  WeatherStation  <- data.frame(wind=v1,
                                RH=rel_hum,
                                temp=t1,
                                radiation=rad_bal,
                                height=z1,
                                lat=lat,
                                long=lon,
                                elev=elevation)

  lv <- hum_evap_heat(t1)  # Spezifische Verdunstungsw?rme
  QE_PM <- lv*(water::hourlyET(WeatherStation, hours=ut, DOY=doy, long.z=long)/3600)*(-1)
  return(QE_PM)
}


#' Latent Heat using Monin-Obukhov length
#'
#' Calculates the latent heat flux using the Monin-Obukhov length. Negative
#' flux signifies flux away from the surface, positive values signify flux
#' towards the surface.
#'
#' @param air_density Air density in kg*m^-3.
#' @param monin Monin-Obukhov length in m.
#' @param moist_gradient Specific moisture gradient (dq/dlog(z) with q in kg/kg)
#' @param ustar Friction velocity in m/s.
#' @param ri Gradient-Richardson-Number.
#' @param t1 Air temperature in degrees C.
#' @param z1 Height of air temperature measurment in m.
#'
#' @return Latent heat flux in W/m^2.
#' @export
latent_monin <- function(air_density, monin, moist_gradient, ustar, ri, t1, z1) {
  lv <- hum_evap_heat(t1)                  # Spezifische Verdunstungswaerme, T in ?C
  k <- 0.4
  s1 <- z1/monin
  busi[ri <= 0] <- 0.95*(1-(11.6*s1))^-0.5      # labil
  busi[ri > 0] <- 0.95+(7.8*s1)                # stabile
  QL <- -1*((air_density*lv*k*ustar)/busi)*1*moist_gradient
  return(QL)
}


#' Latent Heat using Bowen Method
#'
#' Calculates the latent heat flux using the Bowen Method. Negative
#' flux signifies flux away from the surface, positive values signify flux
#' towards the surface.
#'
#' @param t1 Temperature at lower height (e.g. height of anemometer) in degrees C.
#' @param t2 Temperature at upper height in degrees C.
#' @param h1 Relative humidity at lower height (e.g. height of anemometer) in %.
#' @param h2 Relative humidity at upper height in %.
#' @param z1 Lower height of measurement (e.g. height of anemometer) in m.
#' @param z2 Upper height of measurement in m.
#' @param rad_bal Radiation balance in W/m^2.
#' @param soil_flux Soil flux in W/m^2.
#' @param p OPTIONAL. Air pressure in hPa. If not given, elev has to be set.
#' @param elev OPTIONAL. Elevation above sea level in m. If not given, p has to be set.
#'
#' @return Latent heat flux in W/m^2
#' @export
#'
latent_bowen <- function(t1, t2, h1, h2, z1, z2,
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
  return((-rad_bal-soil_flux) / (1+bowen_ratio))
}
