context("Temperature functions")

test_that("Potential temperature works", {
  expect_equal(temp_pot_temp(25, 1000), 299.2745, tolerance=1e-3)
  expect_equal(temp_pot_temp(c(25, NA), c(NA, 1000)), c(NA_integer_, NA_integer_))
})
