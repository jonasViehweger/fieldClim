#' sc PT coefficient (???)
#'
#' @param t1 numeric. Air temperature in degrees celsius
#'
#' @return numeric. sc
sc <- function(t1){
  sc <- 8.5*10^(-7)*(t1+273.15)^2 - 0.0004479*(t1+273.15) + 0.05919
  return(sc)
}

#' lambda PT coefficient (???)
#'
#' @param t1 numeric. Air temperature in degree celsius
#'
#' @return numeric. lambda
lamb <- function(t1){
  lamb <- 0.0004+(0.00041491-0.0004)/(1+(299.44/(t1+273.15))^383.4)
  return(lamb)
}

# Bowen Ration ?ber Temperatur- und  absolute Feuchtegradienten
#' Bowen ratio
#'
#' Calculates bowen ratio
#'
#' @param t1_pot potential temperature (see temp_pot_temp)
#' @param dpot difference in potential temperature between the two measurement
#' heights in degrees
#' @param dah difference in absolute humidity (kg/m^3) between the two measurement heights
#'
#' @return bowen ratio
bowen_ratio <- function(t1_pot, dpot, dah){
  heat_cap <- heat_capacity(t1_pot)
  evap_heat <- hum_evap_heat(t1_pot)
  Bow2 <- (heat_cap*dpot) / (evap_heat*h2)
  return(Bow2)
}

# W?rmekapazit?tsdichte in J/m? K, temp (x1) in ?C
heat_capacity <- function(t1){
  ca <- 1005*(1.2754298-0.0047210538*t1+1.6463585*10^-5*t1)
  return(ca)
}
