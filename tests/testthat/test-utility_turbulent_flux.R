context("Utility Turbulent Flux")

test_that("sc coefficient", {
  expect_equal(sc(22), 0.001038809, tolerance = 1e-3)
})

test_that("lambda coefficient", {
  expect_equal(lamb(22), 0.0004000587, tolerance = 1e-3)
})

test_that("heat capacity", {
  expect_equal(heat_capacity(22.08298), 1177.396, tolerance = 1e-3)
})

test_that("Bowen Coefficient", {
  expect_equal(bowen_ratio(22.08298, -0.1254715, -0.0001465109), 0.4118242, tolerance = 1e-3)
})
