context("Position of climate station")

#Test data
obs_width = 30
obs_height = 13
obs_radius = NULL
dist = 15
min_dist = 145

test_that("pos_min_dist", {
  expect_equal(pos_min_dist(obs_width, obs_height, ring = F, obs_radius), 145, tolerance = 1e0)
})

test_that("pos_max_dist", {
  expect_equal(pos_max_dist(dist, obs_width, obs_height, ring = F), "The climate station is postioned well.")
})

test_that("pos_anemometer_height",{
  expect_equal(pos_anemometer_height(dist, min_dist, obs_height), "The climate station is positioned too close to the obstacle. It needs to repositioned 11.66 m higher.")
})

