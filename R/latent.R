###### Latent Heat Flux PT;  x1 is sc, x2 is lam, x3 is radiation balance, Qg is soil heat flux
lat_taylor_priestly <- function(temp, rad_bal, soil_flux){
  temp <- temp+273.15
  sc <- sc(temp)
  lamb <- lamb(temp)
  alpt <- 1.25
  QE_TP <- alpt*sc*((-rad_bal-soil_flux)/sc+lamb) ;
  return(QE_TP)
}


lat_penman <- function(datetime,
                       v, t1, z1, rel_hum, rad_bal,
                       elevation, lat, lon){
  if(!inherits(datetime, "POSIXt")){
    stop("datetime has to be of class POSIXt.")
  }

  # day of year
  doy <- strftime(datetime, format = "%j")
  # decimal hour
  lt <- as.POSIXlt(datetime)
  ut <- lt$hour + lt$min/60 + lt$sec/3600

  WeatherStation  <- data.frame(wind=v,
                                RH=rel_hum,
                                temp=t1,
                                radiation=rad_bal,
                                height=z1,
                                lat=lat,
                                long=lon,
                                elev=elevation)

  lv <- 2500827-2360*(t1)  # Spezifische Verdunstungsw?rme
  QE_PM <- lv*(water::hourlyET(WeatherStation, hours=ut, DOY=doy, long.z=long)/3600)*(-1)
  return(QE_PM)
}


##################################
###Latenter Waermestrom in W/m? #
##################################
# p       = Luftdichte (kg/m3), x1
# Mo      = Monin Obukhov Laenge X2
# dq/dz   = Spezifischer Feuchtegradient (dq/dlog(z)), x3; in q in kg/kg
# ustern       = Friction velocity , x4
# Ri      x5
# T Temperatur x6
# z hohe = x7
# lv      = Spezifische Verdunstungswaerme (1004,834 J/Kg)

lat_monin <- function(air_density, monin, moist_gradient, ustar, t1, z1) {
  lv <- hum_evap_heat(t1)                  # Spezifische Verdunstungswaerme, T in ?C
  k <- 0.4
  s1 <- x7/x2
  if(x5 < 0) {busi[] <- 0.95*(1-(11.6*s1))^-0.5}      # labil
  if(x5 > 0) {busi <- 0.95+(7.8*s1) }               # stabile
  QL <- -1*((air_density*lv*k*x4)/busi)*1*x3;
  return(QL)
}

# QL_BO Latent heat flux form Bo method, x1 rad balance, x2 bodnw?rmestrom , x3 Bow2
lat_bowen <- function(x1,x2,x3){
  QL_BO <- (-x1-x2) / (1+x3);
  return(QL_BO)
}
