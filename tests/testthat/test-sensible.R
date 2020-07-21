context("Sensible Functions")

test_that("Bowen", {
  expect_equal(sensible_bowen(t1 = 22, t2 = 21,
                              hum1 = 89, hum2 = 88,
                              p1 = 1000, p2 = 1000,
                              z1 = 2, z2 = 10,
                              rad_bal = 400, soil_flux = 40), -128.3465, tolerance = 1e-3)
})
