# fieldClim
Alpha-Version of fieldClim package. <br />
Installation and example of usage:
  * `devtools::install_github("jonasViehweger/fieldClim")`
  * `library(fieldClim)`
  * you can now use the `monthly_climate()` function (for usage see `?monthly_climate()`)
  
Example

    data_complete <- read.csv("yourdata.csv") <br />
    data_january <- data_complete[c(1:7225),] #january <br />
    data_january$datetime <- as.POSIXlt(data_january$datetime) <br />


    example <- monthly_climate(data = data_january, <br />
                            datetime = "datetime", <br />
                            t1 = "Ta_2m", <br />
                            t2 = "Ta_10m", <br />
                            z1 = 2, <br />
                            z2 = 10, <br />
                            v1 = "Windspeed_2m", <br />
                            v2 = "Windspeed_10m", <br />
                            hum1 = "Huma_2m", <br />
                            hum2 = "Huma_10m", <br />
                            rad_bal = "rad_net", <br />
                            sw_bal = "RsNet", <br />
                            lw_bal = "RlNet", <br />
                            surface_type = "Wiese", <br />
                            obs_height = 0.3, <br />
                            soil_flux = "heatflux_soil", <br />
                            elev = 270, <br />
                            lat = 8.683303333333333, <br />
                            lon = 50.84050277777778 <br />
                            )
