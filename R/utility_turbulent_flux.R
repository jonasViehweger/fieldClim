#' sc PT coefficient (???)
#'
#' @param t Air temperature in degrees C.
#'
#' @return sc
sc <- function(t){
  sc <- 8.5*10^(-7)*(t+273.15)^2 - 0.0004479*(t+273.15) + 0.05919
  return(sc)
}

#' lambda PT coefficient (???)
#'
#' @param t Air temperature in degree C
#'
#' @return lambda
lamb <- function(t){
  lamb <- 0.0004+(0.00041491-0.0004)/(1+(299.44/(t+273.15))^383.4)
  return(lamb)
}


#' Bowen-ratio
#'
#' Calculates Bowen-ratio.
#'
#' @param t Air temperature in degrees C.
#' @param dpot Difference in potential temperature between the two measurement
#' heights in degrees Celsius.
#' @param dah Difference in absolute humidity (kg/m^3) between the two measurement heights.
#'
#' @return Bowen-ratio
bowen_ratio <- function(t, dpot, dah){
  heat_cap <- heat_capacity(t)
  evap_heat <- hum_evap_heat(t)
  Bow2 <- (heat_cap*dpot) / (evap_heat*dah)
  return(Bow2)
}

#' Volumetric heat capacity
#'
#' Calculates volumetric heat capacity
#'
#' @param t Air temperature in degrees C.
#'
#' @return Heat capacity density in J/(K*m^3)
#' @export
#'
heat_capacity <- function(t){
  ca <- 1005*(1.2754298-0.0047210538*t+1.6463585*10^-5*t)
  return(ca)
}
