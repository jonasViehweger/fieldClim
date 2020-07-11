#c----Funktion zur Berechnung der Bodenw?rmeleitf?higkeit (W/m K)
# fuer Sand mit Hilfe der Bodenfeuchte x (in Vol-%)
#' Title
#'
#' @param moisture
#' @param texture
#'
#' @return
#' @export
#'
#' @examples
soil_thermal_cond <- function(moisture, texture = "sand") {
  if(texture == "sand"){
    y <- c(0.269,1.46,1.98,2.18,2.31,2.49,2.58)
  } else if(texture == "clay"){
    y <- c(0.276,0.586,1.1,1.43,1.57,1.74,1.95)
  } else {
    stop("Texture not available. Input either 'sand' or 'clay'")
  }
  x <- c(0, 5, 10, 15, 20, 30, 43)

  # linear interpolation of values
  therm_cond <- approx(x, y, xout = moisture, yleft = NA, yright = y[7])
  return(therm_cond$y)
}


#c----Funktion zur Berechnung der Spezifische W?rme pro Volumen [J?m-3?K-1],
# fuer Ton mit Hilfe der Bodenfeuchte x (in Vol-%)
sw_to <- function(x) {

  # Linear Interpolation
  ssand <- c(1.17,1.38,1.59,1.8,2.0,2.42,2.97)
  ston <- c(1.19,1.4,1.61,1.82,2.03,2.45,2.99)

  if (x < 5) {
    dif <- (ston[2]-ston[1])/5
    l_ton <- ston[1]+(dif*x)
  }
  if (x >= 5 & x < 10) {
    dif <- (ston[3]-ston[2])/5
    l_ton <- ston[2]+(dif*(x-5))
  }
  if (x >= 10 & x < 15) {
    dif <- (ston[4]-ston[3])/5
    l_ton <- ston[3]+(dif*(x-10))
  }
  if (x >= 15 & x < 20) {
    dif <- (ston[4]-ston[3])/5
    l_ton <- ston[3]+(dif*(x-10))
  }
  if (x >= 20 & x < 30) {
    dif <- (ston[5]-ston[4])/10
    l_ton <- ston[4]+(dif*(x-20))
  }
  if (x >= 30 & x < 43) {
    dif <- (ston[6]-ston[5])/13
    l_ton <- ston[5]+(dif*(x-30))
  }
  if (x >= 43) {
    l_ton <- 2.58
  }

  sw_to <- l_ton;

  return(sw_to)
}

#c----Funktion zur Berechnung der Spezifische W?rme pro Volumen [J?m-3?K-1],
# fuer Ton mit Hilfe der Bodenfeuchte x (in Vol-%)
sw_sa <- function(x) {

  # Lineare Interpolation

  ssand <- c(1.17,1.38,1.59,1.8,2.0,2.42,2.97)


  if (x < 5) {
    dif <- (ssand[2]-ssand[1])/5
    l_sand <- ssand[1]+(dif*x)
  }
  if (x >= 5 & x < 10) {
    dif <- (ssand[3]-ssand[2])/5
    l_sand <- ssand[2]+(dif*(x-5))
  }
  if (x >= 10 & x < 15) {
    dif <- (ssand[4]-ssand[3])/5
    l_sand <- ssand[3]+(dif*(x-10))
  }
  if (x >= 15 & x < 20) {
    dif <- (ssand[4]-ssand[3])/5
    l_sand <- ssand[3]+(dif*(x-10))
  }
  if (x >= 20 & x < 30) {
    dif <- (ssand[5]-ssand[4])/10
    l_sand <- ssand[4]+(dif*(x-20))
  }
  if (x >= 30 & x < 43) {
    dif <- (ssand[6]-ssand[5])/13
    l_sand <- ssand[5]+(dif*(x-30))
  }
  if (x >= 43) {
    l_sand <- 2.58
  }

  sw_sa <- l_sand;

  return(sw_sa)
}

#c----Funktion zur Berechnung des Bodenw?rmestroms (W/m?) mit
#   x1: Temperaturdifferenz in K
#   x2: Messtrecke (m)
#   x3: Waermeleitfaehigkeit v
QB <- function(x1,x2,x3) {

  QB <- x3*(x1/x2);
  return(QB)

}

#c----Funktion zur Berechnung der D?mpfungstiefe (m) mit
#   x1: Waermeleitf?higkeit
#   x2: Spezifische W?rme pro Volumen

DT <- function(x1,x2) {

  DT <- sqrt((x1*24)/(x2*pi));
  return(DT)

}


