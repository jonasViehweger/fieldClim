###### Sensible HEat Flux PT;  x1 is sc, x2 is lam, x3 is radiation balance, Qg is soil heat flux
sensible_taylor_priestly <- function(temp, rad_bal, soil_flux){
  temp <- temp+273.15
  sc <- sc(temp)
  lamb <- lamb(temp)
  alpt <- 1.25
  QH_TP <- ((1-alpt)*sc+lamb)*(-rad_bal-soil_flux)/(sc+lamb);
  return(QH_TP)
}


##################################
###Fuehlbarer Waermestrom in W/m? #
##################################
# p       = Luftdichte (kg/m3), x1
# mo      = Monin Obukhov Laenge X2
# cp      =
# t_gradient   =  Temperaturgradient (dt/dlog(z)), x3;
# ustern       = Friction velocity , x4
# ri      x5
# z = h?he x6
QH <- function(air_density, monin, t_gradient, ustar, ri, z1) {
  cp <- 1004.834
  k <- 0.4
  s1 <- z/monin
  ri[ri>0] <- 0.95+(7.8*s1)
  if(ri < 0) {busi <- 0.95*(1-(11.6*s1))^-0.5}      # labil
  if(ri > 0) {busi <- 0.95+(7.8*s1) }               # stabile
  QH <- -1*((air_density*cp*k*ustar)/busi)*1*t_gradient
  return(QH)
}

# QH_BO Snesible heat flux form Bo method, x1 rad balance, x2 bodnw?rmestrom , x3 Bow2
QH_BO <- function(x1,x2,x3){
  QH_BO <- (-x1-x2) * (x3 / (1+x3));
  return(QH_BO)
}
