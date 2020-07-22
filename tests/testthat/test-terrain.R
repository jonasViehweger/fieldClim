context("Terrain")

test_that("terr_sky_view", {
  expect_equal(terr_sky_view(20, valley = T), 0.9396926, tolerance = 1e-4)
  expect_equal(terr_sky_view(20, valley = F), 0.9698463, tolerance = 1e-4)
})
