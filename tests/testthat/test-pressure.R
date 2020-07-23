context("Pressure")

#Test data
elev = 200
t = 20.8
p = 989.9613

test_that("pres_p", {
  expect_equal(pres_p(elev, t), 989.9613, tolerance = 1e-4)
})

test_that("pres_air_density", {
  expect_equal(pres_air_density(p, t), 1.1732, tolerance = 1e-4)
})
