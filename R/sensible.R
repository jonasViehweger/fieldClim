###### Sensible HEat Flux PT;  x1 is sc, x2 is lam, x3 is radiation balance, Qg is soil heat flux
QH_TP <- function(temp, rad_bal, soil_flux){
  temp <- temp+273.15
  sc <- sc(temp)
  lamb <- lamb(temp)
  alpt <- 1.25
  QH_TP <- ((1-alpt)*sc+lamb)*(-rad_bal-soil_flux)/(sc+lamb);
  return(QH_TP)
}
