#c----Funktion zur Berechnung des Exzentrizit?tsfaktor
exz <- function(n) {
  x=2.*pi*(n-1)/365.
  exz=1.00011+0.034221*cos(x)+0.00128*sin(x)+0.000719*cos(2*x)+0.000719*sin(2.*x);
  return(exz)
}



#--- Funktion zur Berechnung von Sonnenhoehe
#     n: Julianischer Tag
#     ut: Dezimale Ortszeit
#     gb, gl: Geographische Breite und L?nge
#     f: Faktor zur Umrechnung von Grad in Radiant
#     gbr, glr: Breite und L?nge in Radiant
#     m: Mittlere Anomalie der Sonne
#     t: Mittlere Sonnenzeit
#     h: Stundenwinkel
#     del: Geoz. schinb. eklipt. L?nge der Sonne
#     zt: Zeitgleichung
#     sde: Sinus Deklination der Sonne

sh <- function(n,ut,gb,gl) {
  f <- pi/180   # Winkel in Radiant Faktor
  #---------------------------Anweisungsteil
  gbr <- gb*f
  glr <- gl*f
  #---------------------------Mittlere Sonnenzeit
  t <- ut+gl/15.           # In Stunden
  #---------------------------Stundenwinkel in Radiant
  m <- 356.6+0.9856*n      # in Grad
  m <- m*f                 # in Radiant
  zt <- 0.1644*sin(2.*glr) - 0.1277*sin(m) # in Stunden
  h <- (15.*f)*(t+zt-12.)  # In Radiant
  #---------------------------Scheinb. geoz etc.
  del <- 279.3*f+0.9856*f*n+1.92*f*sin(356.6*f+0.9856*f*n) #  in Radiant
  #      del=del*f             #  in Radiant
  #---------------------------Sinus Deklination der Sonne
  sde <- sin(23.44*f)*sin(del)     #  In Radiant
  #---------------------------Sonnenh?he
  shh <- sin(gbr)*sde+cos(gbr)*cos(asin(sde))*cos(h)
  sh <- asin(shh)/f;
  return(sh)
}


### Function Sonnenazimuth
saz <- function(n,ut,gb,gl, shh) {
  f <- pi/180   # Winkel in Radiant Faktor
  gbr <- gb*f
  glr <- gl*f
  #---------------------------Mittlere Sonnenzeit
  t <- ut+gl/15.           # In Stunden
  #---------------------------Stundenwinkel in Radiant
  m <- 356.6+0.9856*n      # in Grad
  m <- m*f                 # in Radiant
  zt <- 0.1644*sin(2.*glr) - 0.1277*sin(m) # in Stunden
  h <- (15.*f)*(t+zt-12.)  # In Radiant
  #---------------------------Scheinb. geoz etc.
  del <- 279.3*f+0.9856*f*n+1.92*f*sin(356.6*f+0.9856*f*n) #  in Radiant
  #      del=del*f             #  in Radiant
  #---------------------------Sinus Deklination der Sonne
  sde <- sin(23.44*f)*sin(del)     #  In Radiant
  #---------------------------Sonnenazimut
  if(t <= 12) {
    saz=(sde*cos(gbr)-cos(asin(sde))*sin(gbr)*cos(h))/cos(shh)
    saz=acos(saz)
  } else {
    saz=(sde*cos(gbr)-cos(asin(sde))*sin(gbr)*cos(h))/cos(shh)
    saz=360.*f-acos(saz) }
  saz <- saz/f;
  return(saz)
}
