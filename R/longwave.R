### GEGENSTRAHLUNG ####

#---> Zus?tzliche Dateneingabe Atmosph?re

rf <- 80                    # Relative Feuchte in %
o <- 5.6693 * 10^-8         # W * m^-2 * K^-4 #Stefan-Boltzmann-Radiation-contant
ot <-  15                   # Oberfl?chentemperatur

#---> Zus?tzliche Dateneingabe

em <- 0.95                 # Emissionsgrad Oberfl?che; Gras ~ 0.95'


#################################################
#           Rechnungen
#################################################


svp(t)
steam_pressure(rf,svp(t))  # Dampdfdruck in hPa
eat(svp(t),t,p)            # Emissivitaet Luft

print("Die atmosph?rische Gegenstrahlung betr?gt in W/m?")
GS(eat(svp(t),t,p),o,t+273.15)

print("Die langwellige Ausstrahlung betr?gt in W/m?")
L_out <- em*o*(ot+273.15)**4
L_out

print("Die effektive Ausstrahlung = langwellige Strahlungsbilanz betr?gt in W/m?")
A_eff <- (L_out - GS(eat(svp(t),t,p),o,t+273.15))
A_eff


###############################################
########### Modifikation Gelaende IR
################################################

G_topo <- (GS(eat(svp(t),t,p),o,t+273.15)*sky)+ (L_out*ter_v)
print("Die atmosph?risch-topographische Gegenstrahlung betr?gt in W/m?")
G_topo

A_bil_topo <- L_out - G_topo

print("Die atmosph?risch-topographische Strahlungsbilanz betr?gt in W/m?")
A_bil_topo

# Funktion Emissionsfaktor Luft
# steam_pressure (hPa), Temperature_atmosphere (?C), air_pressure (hPa), z: altitude ground
eat <- function(sp,tat,p,z){
  t_over <- tat*(0.0065*z)
  eat <-((1.24*sp/tat)**1/7)*(p/1013.25);
  return(eat)
}

# Funktion amtosph?rische Gegenstrahlung
# eat: emmissionsfaktor luft
# o:   stefan-boltzmann-konstant (whhy)
# tat: temperatur atmosphäre
gs <- function(eat,o,tat){
  gs <- eat*o*tat**4;
  return(gs)
}
