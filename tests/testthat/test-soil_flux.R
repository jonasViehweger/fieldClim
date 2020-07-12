context("Soil functions")

# values for linear interpolation:
sand_cond <- c(0.269,1.46,1.98,2.18,2.31,2.49,2.58)
clay_cond <- c(0.276,0.586,1.1,1.43,1.57,1.74,1.95)
moisture <- c(0, 5, 10, 15, 20, 30, 43)

test_that("soil_thermal_cond", {
  expect_equal(soil_thermal_cond(moisture, texture = "sand"), sand_cond)
  expect_equal(soil_thermal_cond(moisture, texture = "clay"), clay_cond)
  expect_equal(soil_thermal_cond(c(NA, -10), texture = "sand"), c(NA, NA_integer_))
  expect_equal(soil_thermal_cond(c(NA, -10), texture = "clay"), c(NA, NA_integer_))
  expect_error(soil_thermal_cond(moisture, texture = "error"))
  expect_warning(soil_thermal_cond(c("Hallo", "13")))
})

# values for linear interpolation:
sand_cap <- c(1.17,1.38,1.59,1.8,2.0,2.42,2.97)
clay_cap <- c(1.19,1.4,1.61,1.82,2.03,2.45,2.99)

test_that("soil_heat_cap", {
  expect_equal(soil_heat_cap(moisture, texture = "sand"), sand_cap)
  expect_equal(soil_heat_cap(moisture, texture = "clay"), clay_cap)
  expect_equal(soil_heat_cap(c(NA, -10), texture = "sand"), c(NA, NA_integer_))
  expect_equal(soil_heat_cap(c(NA, -10), texture = "clay"), c(NA, NA_integer_))
  expect_error(soil_heat_cap(moisture, texture = "error"))
  expect_warning(soil_heat_cap(c("Hallo", "13")))
})
