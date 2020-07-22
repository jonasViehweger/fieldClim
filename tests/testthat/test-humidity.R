context("Humidity")

# Input Values
t1 <- 22
t2 <- 21
t_pot <- 296.2632
hum1 <- 89
hum2 <- 88
z1 <- 2
z2 <- 10
p_vapor <- 23.83956
p1 <- 1000
p2 <- 1000

test_that("hum_sat_vapor_pres", {
  expect_equal(hum_sat_vapor_pres(t = t1), 26.78602, tolerance = 1e-3)
})

test_that("hum_vapor_pres", {
  expect_equal(hum_vapor_pres(hum = hum1, t = t1), p_vapor, tolerance = 1e-3)
})

test_that("hum_specific", {
  expect_equal(hum_specific(p_vapor = p_vapor, p = 1000), 0.0148282, tolerance = 1e-4)
})

test_that("hum_absolute", {
  expect_equal(hum_absolute(p_vapor = p_vapor, t_pot = t_pot), 0.01743569, tolerance = 1e-4)
})

test_that("hum_evap_heat", {
  expect_equal(hum_evap_heat(t = t1), 2448616, tolerance = 1e-4)
})

test_that("hum_precipitable_water", {
  expect_equal(hum_precipitable_water(1000, 20.8, 200), 3.114731, tolerance = 1e-4)
})

test_that("hum_moisture_gradient", {
  expect_equal(hum_moisture_gradient(hum1, hum2,
                                     t1, t2,
                                     p1, p2,
                                     z1, z2), -0.0005018968, tolerance = 1e-6)
})
