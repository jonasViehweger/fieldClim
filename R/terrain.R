################## Eingaben Gelaende

h  <- 200           # Gelaendehoehe in m
sky <- 0.8          # Himmelssichtfaktor (sky view factor 0..1); 1, wenn keine Horizontueberschattung; wenn nicht bekannt, rechnen
hex <- 180          # Expositionswinkel 0 = N, 180? = S
hn <- 20            # Hangneigung in Grad (0...90?)
albedo <- 0.15      # Albedo in 0...1

# fl <- 2           # Optionale Rechnung wenn nicht bekannt
# sky <-  sky_v(hn,fl)
# sky

ter_v <- 1 - sky     # Gelaendesichtfaktor (terrain view)
ter_v

############### Schattenl?nge rechnen; hier auch f?r das 2 m Level des Strahlungssensors

# Zuerst muss der Azimuth gepr?ft werden und dann Entfernung und H?he festgelegt werden;
# machen wir jetzt einmal h?ndisch
wert2  # hier mit Testwerten bei Az = 165?, h ca 20 m, dist ca 50 m
wert   # Sonnenhoehe

hh <- 20
di <- 50

terr_shadow <- function(sol_elevation, sol_azimuth, dist, height, offset = 0){
  return(hh * sin((90-sol_elevation)*f) / sin(sol_elevation*f))
}

sl_ground <- hh * sin((90-wert)*f) / sin(wert*f)                # Wert ist die Sonnenh?he
sl_ground
sl_2m <- (hh-2) * sin((90-wert)*f) / sin(wert*f)                # Wert ist die Sonnenh?he
sl_2m

if (sl_2m > di) {
  print("Keine Direktstrahlung, da Pyranometer im Schlagschatten liegt")
  sol_dir <- 0
}

#---------Funktionsunterprogramm zur Berechnung des sky view faktors fuer Taeler und Haenge
# flag 1 = Hang, 2 = Tal
#' Sky view factor
#'
#' @param slope Inclination of slope in degrees
#' @param valley If the position is in a valley (TRUE) or on a slope (FALSE)
#'
#' @return sky view factor from 0-1
#' @export
#'
#' @examples
terr_sky_view <- function(slope, valley = F) {
  f <- pi/180                                 # Winkel in Radiant Faktor
  if(valley) return((1+cos(slope*f))/2.0)
  return(cos(slope*f))
}
