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
                                      albedo = test_data$Albedo)


out_list <- list(location_properties = list(latitude = 50.840503,
                                            longitude = 8.683300,
                                            elevation = 270,
                                            surface_type = "Wiese",
                                            obs_height = 0.3),
                 properties = list(z1 = 2,
                                   z2 = 10),
                 measurements = list(datetime = test_data$datetime,
                                     t1 = test_data$Ta_2m,
                                     t2 = test_data$Ta_10m,
                                     v1 = test_data$Windspeed_2m,
                                     v2 = test_data$Windspeed_10m,
                                     hum1 = test_data$Huma_2m,
                                     hum2 = test_data$Huma_10m,
                                     p1 = NULL,
                                     p2 = NULL,
                                     sw_in = NULL,
                                     sw_out = NULL,
                                     lw_in = NULL,
                                     lw_out = NULL,
                                     soil_flux = NULL))

