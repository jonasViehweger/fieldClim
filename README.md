# fieldClim

The `fieldClim` package was originally designed as a course project to the course `Geländeklimatologie`, held by Prof. Dr. Jörg Bendix at the Philipps-University of Marburg in summer term 2020. Thus, the calculations and formulas of this package are based on this course, as well as the book `Geländeklimatologie` (Field climatology) by Jörg Bendix (2004; ISBN 978-3-443-07139-4).

`fieldClim` is designed as a handy tool, that lets you calculate various weather and micro-climate conditions, based on the measurements of a weather station. It lets you create a `weather_station`-object, that can then be used to call most of the functions without the necessity of further specify input variables. In addition, all functions can also be called by manually inputting the needed variables, if the user whishes to do so.

### Installation and example of usage:
  * install package `water` first
  * `devtools::install_github("jonasViehweger/fieldClim")`
  * `library(fieldClim)`
  * you can now use the `build_weather_station()` function (for usage see `?build_weather_station()`)
  
### Example
If you got your weather station data and and simply want to get a quick overview of the overall micro-climatic conditions at the location of your weather station, such as the atmospheric stability and the latent and sensible heat flows (for a list of all output parameters, see `?as.data.frame.weather_station`), you just need three functions: `build_weather_station()`, `turb_flux_calc()` and `as.data.frame()`.

```r
    # Load package
    library(fieldClim)
    # Load sample data
    ws <- get(data(weather_station_example_data, package="fieldClim"))

    # if your datetime coloumn is not in the POSIXlt-format
    # you need to convert it before you continue
    ws$datetime <- as.POSIXlt(ws$datetime)
    
    # now you can build a "weather_station"-object
    test_station <- build_weather_station(lat = 50.840503,
                                      lon = 8.6833,
                                      elev = 270,
                                      surface_type = "Meadow",
                                      obs_height = 0.3, # obstacle height
                                      z1 = 2, # measurement heights
                                      z2 = 10,
                                      datetime = ws$datetime,
                                      t1 = ws$t1, # temperature
                                      t2 = ws$t2,
                                      v1 = ws$v1, # windspeed
                                      v2 = ws$v2,
                                      hum1 = ws$hum1, # humidity
                                      hum2 = ws$hum2,
                                      sw_in = ws$rad_sw_in, # shortwave radiation
                                      sw_out = ws$rad_sw_out,
                                      lw_in = ws$rad_lw_in, # longwave radiation
                                      lw_out = ws$rad_lw_out,
                                      soil_flux = ws$heatflux_soil)

    # after creating the "weather_station"-object, 
    # you can calculate and add the turbulent fluxes
    station_turbulent <- turb_flux_calc(test_station)
    
    # for a convenient output of the calculated data, you can convert it into a data frame
    out <- as.data.frame(station_turbulent)
``` 
For a more comprehensive example, you might have a look at the [vignette](https://jonasviehweger.github.io/fieldClim/articles/fieldClim.html).

