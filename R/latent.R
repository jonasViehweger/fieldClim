#' Latent Heat Priestley-Taylor Method
#'
#' Calculates the latent heat flux using the Priestley-Taylor method. Negative
#' heat flux signifies flux away from the surface, positive values signify flux
#' towards the surface.
#'
#' @param t Air temperature in degrees Celsius.
#' @param rad_bal Radiation balance in W/m^2.
#' @param soil_flux Soil flux in W/m^2.
#' @param coefficient Priestley-Taylor coefficient. Default is for open water.
#'
#' @return Latent heat flux in W/m^2.
#' @export
#'
latent_priestley_taylor <- function(t, rad_bal, soil_flux, coefficient = 1.25){
  sc <- sc(t)
  lamb <- lamb(t)
  alpt <- coefficient
  QE_TP <- alpt*sc*((-1*rad_bal-soil_flux)/sc+lamb)
  return(QE_TP)
}


#' Latent Heat Penman Method
#'
#' Calculates the latent heat flux using the Penman-Monteith equation. Negative
#' heat flux signifies flux away from the surface, positive values signify flux
#' towards the surface.
#'
#' @param datetime POSIXt object (POSIXct, POSIXlt).
#' See [base::as.POSIXlt] and [base::strptime] for conversion.
#' @param v Wind velocity in m/s.
#' @param t Temperature in degrees C
#' @param hum Relative humidity in %.
#' @param z Height of measurement for t, v in m.
#' @param rad_bal Radiation balance in W/m^2.
#' @param elev Elevation above sea level in m.
#' @param lat Latitude in decimal degrees.
#' @param lon Longitude in decimal degrees.
#'
#' @return Latent heat flux in W/m^2.
#' @import water
#' @export
#'
latent_penman <- function(datetime,
                       v, t, hum, z = 2, rad_bal,
                       elev, lat, lon){
  if(!inherits(datetime, "POSIXt")){
    stop("datetime has to be of class POSIXt.")
  }

  # day of year
  doy <- as.numeric(strftime(datetime, format = "%j"))
  # decimal hour
  lt <- as.POSIXlt(datetime)
  ut <- lt$hour + lt$min/60 + lt$sec/3600

  WeatherStation  <- data.frame(wind=v,
                                RH=hum,
                                temp=t,
                                radiation=rad_bal,
                                height=z,
                                lat=lat,
                                long=lon,
                                elev=elev)

  lv <- hum_evap_heat(t)  # Spezifische Verdunstungsw?rme
  QE_PM <- lv*(water::hourlyET(WeatherStation, hours=ut, DOY=doy)/3600)*(-1)
  return(QE_PM)
}


#' Latent Heat using Monin-Obukhov length
#'
#' Calculates the latent heat flux using the Monin-Obukhov length. Negative
#' flux signifies flux away from the surface, positive values signify flux
#' towards the surface.
#'
#'
#' @param hum1 Relative humidity at lower height in %.
#' @param hum2 Relative humidity at upper height in %.
#' @param t1 Air temperature at lower height in degrees C.
#' @param t2 Air temperature at upper height in degrees C.
#' @param p1 Pressure at lower height in hPa.
#' @param p2 Pressure at upper height in hPa.
#' @param z1 Lower height of measurement in m.
#' @param z2 Upper height of measurement in m.
#' @param monin Monin-Obukhov-Length in m.
#' @param ustar Friction velocity in m/s.
#' @param grad_rich_no Gradient-Richardson-Number.
#'
#' @return Latent heat flux in W/m^2.
#' @export
latent_monin <- function(hum1, hum2, t1, t2, p1, p2, z1 = 2, z2 = 10,
                         monin, ustar, grad_rich_no) {

  moist_gradient <- hum_moisture_gradient(hum1, hum2, t1, t2, p1, p2, z1, z2)
  air_density <- pres_air_density(p1, t1)
  lv <- hum_evap_heat(t1)
  k <- 0.4
  s1 <- z2/monin
  busi <- rep(NA, length(grad_rich_no))
  for(i in 1:length(busi)){
    if(is.na(grad_rich_no[i])){busi[i] <- NA}
    else if(grad_rich_no[i] <= 0){busi[i] <- 0.95*(1-(11.6*s1[i]))^-0.5}
    else if(grad_rich_no[i] > 0){busi[i] <- 0.95+(7.8*s1[i])}
  }
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
#' @param hum1 Relative humidity at lower height (e.g. height of anemometer) in %.
#' @param hum2 Relative humidity at upper height in %.
#' @param p1 Air pressure at lower height in hPa.
#' @param p2 Air pressure at upper height in hPa.
#' @param z1 Lower height of measurement (e.g. height of anemometer) in m.
#' @param z2 Upper height of measurement in m.
#' @param rad_bal Radiation balance in W/m^2.
#' @param soil_flux Soil flux in W/m^2.
#'
#' @return Latent heat flux in W/m^2
#' @export
#'
latent_bowen <- function(t1, t2, hum1, hum2, p1, p2, z1 = 2, z2 = 10,
                         rad_bal, soil_flux){

  # Calculating potential temperature delta
  t1_pot <- temp_pot_temp(t1, p1)
  t2_pot <- temp_pot_temp(t2, p2)
  dpot <- (t2_pot-t1_pot) / (z2-z1)

  # Calculating absolute humidity delta
  af1 <- hum_absolute(hum_vapor_pres(hum1, t1), t1_pot)
  af2 <- hum_absolute(hum_vapor_pres(hum2, t2), t2_pot)
  dah <- (af2-af1) / (z2-z1)

  # Calculate bowen ratio
  bowen_ratio <- bowen_ratio(t1, dpot, dah)
  out <- (-1*rad_bal-soil_flux) / (1+bowen_ratio)
  return(out)
}
