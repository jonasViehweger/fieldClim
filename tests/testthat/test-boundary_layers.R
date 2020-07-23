context("Boundary layers")

#Test data
dist = 200
ustar = 0.1596604
v = 3.0
temp_change_dist = 200
pt_upwind = 284
t_pot = 280
lapse_rate = -0.006


test_that("bound_mech_low",{
  expect_equal(bound_mech_low(dist), 4.242641, tolerance =1e-6)
})

test_that("bound_mech_avg",{
  expect_equal(bound_mech_avg(dist), 6.081118, tolerance =1e-6)
})

test_that("bound_thermal_avg",{
  expect_equal(bound_thermal_avg(ustar,v,temp_change_dist,pt_upwind,t_pot,lapse_rate), 19.43324, tolerance =1e-2)
})

