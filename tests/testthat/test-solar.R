context("Solar")

datetime <- as.POSIXlt("2018-9-29 11:12:00", tz = "CET")
lat <- 50.84050277777778
lon <- 8.683303333333333

test_that("sol_eccentricity", {
  expect_equal(sol_eccentricity(datetime), 0.9965642, tolerance = 1e-5)
})

test_that("sol_elevation", {
  expect_equal(sol_elevation(datetime, lat, lon), 36.98267, tolerance = 1e-5)
})

test_that("sol_azimuth", {
  expect_equal(sol_azimuth(datetime, lat, lon), 179.1576, tolerance = 1e-5)
})
