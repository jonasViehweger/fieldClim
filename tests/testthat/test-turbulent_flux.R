context("Turbulent Flux")

# Test data

ah <- 10             # Messhoehe
h <- 1.2   # Obstacle height in m
vh <- 1.2  # Vegetation height in m
v <- 3.5   # wind velocity
z0 <- 0.92 # roughness length
rh1 <- 89     # Relative Humidity Hoehe 1
rh2 <- 88     # Relative Humidity Hoehe 2

p0 <- 1013.25  # Referenzdruck Meeresniveau in hPa
p <- 1000      # Gemessener Druck in hPa
t1 <- 22        # Lufttemperatur auf 2 m
t2<- 21        # Lufttemperatur auf 10 m
z1 <- 2        # Erste Messhoehe Meter
z2 <- 10       # 2te MEsshoehe
v1 <- 1        # m/Sec Windgeschwindigkeit Hoehe 1
v2 <- 2.3      # m/Sec Windgeschwindigkeit Hoehe 2
ludi <- 1.225  # Luftdiche Meeresniveau kg/m?


test_that("turb_flux_monin", {
  expect_equal(turb_flux_monin(stability = "labil", z1 = 2, z2 = 10, z0, v1, v2, t1, t2), -63.55804)
})
