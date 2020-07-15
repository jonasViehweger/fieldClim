###### Latent Heat Flux PT;  x1 is sc, x2 is lam, x3 is radiation balance, Qg is soil heat flux
QE_TP <- function(temp, rad_bal, soil_flux){
  temp <- temp+273.15
  sc <- sc(temp)
  lamb <- lamb(temp)
  alpt <- 1.25
  QE_TP <- alpt*sc*((-rad_bal-soil_flux)/sc+lamb) ;
  return(QE_TP)
}

# Wollen wir "water" requiren damit wir Penman ausrechnen koennen?
# lv <- 2500827-2360*(t)  # Spezifische Verdunstungsw?rme
# QE_PM <- lv* (hourlyET(WeatherStation, hours=10.5, DOY=363, long.z=71.635)/3600)*(-1)
