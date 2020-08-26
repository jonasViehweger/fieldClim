test_data <- weather_station_example_data

test_data$datetime <- as.POSIXct(test_data$datetime)
test_data$Albedo <- as.character(test_data$Albedo)
test_data$Albedo <- as.numeric(test_data$Albedo)

test_station <- build_weather_station(z1 = 2,
                                      z2 = 10,
                                      datetime = test_data$datetime,
                                      t1 = test_data$Ta_2m,
                                      t2 = test_data$Ta_10m,
                                      v1 = test_data$Windspeed_2m,
                                      v2 = test_data$Windspeed_10m,
                                      hum1 = test_data$Huma_2m,
                                      hum2 = test_data$Huma_10m,
                                      albedo = test_data$Albedo,
                                      t_surface = test_data$Ts,
                                      texture = "clay",
                                      depth1 = 0.1,
                                      depth2 = 0.3,
                                      ts1 = test_data$Ts,
                                      ts2 = test_data$Ts-0.1,
                                      moisture = test_data$water_vol_soil)



