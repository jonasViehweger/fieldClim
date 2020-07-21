context("Humidity")

# Input Values
t <- 22
t_pot <- 296.2632
hum <- 89
p_vapor <- 23.83956

test_that("hum_sat_vapor_pres", {
  expect_equal(hum_sat_vapor_pres(t = t), 26.78602, tolerance = 1e-3)
})

test_that("hum_vapor_pres", {
  expect_equal(hum_vapor_pres(hum = hum, t = t), p_vapor, tolerance = 1e-3)
})

test_that("hum_specific", {
  expect_equal(hum_specific(p_vapor = p_vapor, p = 1000), 0.0148282, tolerance = 1e-4)
})

test_that("hum_absolute", {
  expect_equal(hum_absolute(p_vapor = p_vapor, t_pot = t_pot), 0.01743569, tolerance = 1e-4)
})

test_that("hum_evap_heat", {
  expect_equal(hum_evap_heat(t = t), 2448616, tolerance = 1e-4)
})

