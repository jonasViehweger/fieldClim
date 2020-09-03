library(fieldClim)

## code to prepare `weather_station_example_data` dataset goes here

day <- read.csv(file.path("data-raw", "caldern_weather_station.csv"))

#calculate surface temperature from lw_out:

day$t_surface <- (day$LUpCo/5.67e-8)^(1/4)-273.15


# select necessary columns

sel_columns <- c("datetime", "Ta_2m", "Huma_2m", "Ta_10m", "Huma_10m",
                 "rad_sw_in", "rad_sw_out", "rad_lw_in", "rad_lw_out",
                 "RsNet", "RlNet", "Albedo", "rad_net",
                 "water_vol_soil", "Ts", "heatflux_soil",
                 "Windspeed_2m", "Windspeed_10m", "t_surface")
new_names <-  c("datetime", "t1", "hum1", "t2", "hum2",
                "rad_sw_in", "rad_sw_out", "rad_lw_in", "rad_lw_out",
                "rad_sw_bal", "rad_lw_bal", "albedo", "rad_bal",
                "water_vol_soil", "ts1", "heatflux_soil",
                "v1", "v2", "t_surface")

day_selected <- day[,sel_columns]
colnames(day_selected) <- new_names


# clean data
day_selected$datetime <- as.POSIXlt(day$datetime)

# Check for any non numeric columns
#colnames(Filter(Negate(is.numeric), day_selected))

# handle those columns
day_selected$albedo <- as.numeric(day_selected$albedo)


# Add air pressure for fun

day_selected$p1 <- pres_p(270, day_selected$t1)
day_selected$p2 <- pres_p(270, day_selected$t2)

weather_station_example_data <- day_selected

usethis::use_data(weather_station_example_data, overwrite = TRUE)
