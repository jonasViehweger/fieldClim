context("transmittance")

################# Notwendige Eingaben Atmosph?re

p <- 1000                   # Air pressure in hPa; wenn bekannt, angeben; sonst unten berechnen
oz <- 0.35                  # average Ozon columnar values 0.35 cm
t <- 20.8                   # Temperatur in ?C
vis <- 40                   # horizontale Sichtweite in km
datetime <- as.POSIXlt("2018-9-29 10:12:00", tz = "CET")
h  <- 200           # Gelaendehoehe in m


test_that("trans_air_mass_rel", {
  expect_equal(trans_air_mass_rel(sol_elevation = 36.98267), 1.402452, tolerance = 1e-5)
})

test_that("trans_air_mass_abs", {
  expect_equal(trans_air_mass_abs(air_mass_rel = 1.402452, p), 1.384112, tolerance = 1e-5)
})

test_that("trans_rayleigh", {
  expect_equal(trans_rayleigh(1.384112), 0.8885926, tolerance = 1e-5)
})

test_that("trans_ozone", {
  expect_equal(trans_ozone(1.402452, oz), 0.9794733, tolerance = 1e-5)
})

test_that("trans_vapor", {
  expect_equal(trans_vapor(1.402452, precipitable_water = 3.114731), 0.8671652, tolerance = 1e-5)
})

test_that("trans_aerosol", {
  expect_equal(trans_aerosol(1.384112, vis = vis), 0.8139098, tolerance = 1e-5)
})

test_that("trans_gas", {
  expect_equal(trans_gas(1.384112), 0.9862751, tolerance = 1e-5)
})

test_that("trans_total", {
  expect_equal(trans_total(sol_elevation = 36.98267, t = t, elev = h,
                           oz = oz, vis = vis, p = p),
               0.6058589, tolerance = 1e-6)
})
