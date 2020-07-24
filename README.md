# fieldClim
Alpha-Version of fieldClim package.
Installation and example of usage:
  * *devtools::install_github("jonasViehweger/fieldClim")*
  * *library(fieldClim)*
  * you can now use the *monthly_climate* function (for usage see *?monthly_climate()*)
  
Example
data_complete <- read.csv("yourdata.csv") /n
data_january <- data_complete[c(1:7225),] #january /n
data_january$datetime <- as.POSIXlt(data_january$datetime) /n


exxample <- monthly_climate(data = data_january, /n
                        datetime = "datetime", /n
                        t1 = "Ta_2m", /n
                        t2 = "Ta_10m", /n
                        z1 = 2, /n
                        z2 = 10, /n
                        v1 = "Windspeed_2m", /n
                        v2 = "Windspeed_10m", /n
                        hum1 = "Huma_2m", /n
                        hum2 = "Huma_10m", /n
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
