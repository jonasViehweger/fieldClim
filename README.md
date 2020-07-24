# fieldClim
Alpha-Version of fieldClim package.
Installation and example of usage:
  * *devtools::install_github("jonasViehweger/fieldClim")*
  * *library(fieldClim)*
  * you can now use the *monthly_climate* function (for usage see *?monthly_climate()*)
  
Example
data_complete <- read.csv("yourdata.csv")
data_january <- data_complete[c(1:7225),] #january
data_january$datetime <- as.POSIXlt(data_january$datetime)


exxample <- monthly_climate(data = data_january,
                        datetime = "datetime",
                        t1 = "Ta_2m",
                        t2 = "Ta_10m",
                        z1 = 2,
                        z2 = 10,
                        v1 = "Windspeed_2m",
                        v2 = "Windspeed_10m",
                        hum1 = "Huma_2m",
                        hum2 = "Huma_10m",
                        rad_bal = "rad_net",
                        sw_bal = "RsNet",
                        lw_bal = "RlNet",
                        surface_type = "Wiese",
                        obs_height = 0.3,
                        soil_flux = "heatflux_soil",
                        elev = 270,
                        lat = 8.683303333333333,
                        lon = 50.84050277777778
                        )
