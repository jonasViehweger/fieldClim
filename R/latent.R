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

QL <- function(x1,x2,x3,x4,x5,x6,x7) {
  lv <- 2500827-(2360*x6)                  # Spezifische Verdunstungswaerme, T in ?C
  k <- 0.4
  s1 <- x7/x2
  if(x5 < 0) {Busi <- 0.95*(1-(11.6*s1))^-0.5}      # labil
  if(x5 > 0) {Busi <- 0.95+(7.8*s1) }               # stabile
  QL <- -1*((x1*lv*k*x4)/Busi)*1*x3;
  return(QL)
}

# QL_BO Latent heat flux form Bo method, x1 rad balance, x2 bodnw?rmestrom , x3 Bow2
QL_BO <- function(x1,x2,x3){
  QL_BO <- (-x1-x2) / (1+x3);
  return(QL_BO)
}
