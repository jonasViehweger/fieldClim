###### KOeffizient sc PT; temp in K ist x1
#' sc PT coefficient (???)
#'
#' @param temp numeric. Air temperature in K
#'
#' @return numeric. sc
sc <- function(temp){
  sc <- 8.5*10^(-7)*temp^2 - 0.0004479*temp + 0.05919
  return(sc)
}

###### KOeffizient lambda PT; temp in K ist x1
#' lambda PT coefficient (???)
#'
#' @param temp numeric. Air temperature in K
#'
#' @return numeric. lambda
lamb <- function(temp){
  lamb <- 0.0004+(0.00041491-0.0004)/(1+(299.44/temp)^383.4)
  return(lamb)
}

# Bowen Ration ?ber Temperatur- und  absolute Feuchtegradienten
Bow2 <- function(x1,x2,x3,x4){
  Bow2 <- (x1*x3) / (x2*x4);
  return(Bow2)
}
