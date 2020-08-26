test_data <- weather_station_example_data

#test_data$datetime <- as.POSIXct(test_data$datetime)
#test_data$albedo <- as.character(test_data$albedo)
#test_data$albedo <- as.numeric(test_data$albedo)

#test_data$heatflux_soil <- as.character(test_data$heatflux_soil)
#test_data$heatflux_soil <- as.numeric(test_data$heatflux_soil)

test_station <- build_weather_station(z1 = 2,
                                      z2 = 10,
                                      datetime = test_data$datetime,
                                      t1 = test_data$t1,
                                      t2 = test_data$t2,
                                      v1 = test_data$v1,
                                      v2 = test_data$v2,
                                      hum1 = test_data$hum1,
                                      hum2 = test_data$hum2,
                                      albedo = test_data$albedo,
                                      t_surface = test_data$t_surface,
                                      texture = "clay",
                                      depth1 = 0.1,
                                      depth2 = 0.3,
                                      ts1 = test_data$t_surface,
                                      ts2 = test_data$t_surface-0.1,
                                      moisture = test_data$water_vol_soil)

test_dataframe <- as.data.frame(test_station)

test_monin <- turb_flux_monin(weather_station)

sensible_priestley_taylor(weather_station)
latent_priestley_taylor(weather_station)

sensible_bowen(weather_station)
latent_bowen(weather_station)

sensible_monin(weather_station)
latent_monin(weather_station)

library(water)
latent_penman(weather_station)
