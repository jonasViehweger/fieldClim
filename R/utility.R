#' Check Availability
#'
#' Checks availability of passed properties in the weather station object.
#' If property is NULL, aborts with error.
#'
#' @param weather_station Object of class weather_station.
#' @param ... Strings of properties to check.
#' @return Absolutely nothing
#' @export
#'
check_availability <- function(weather_station, ...){
  unlisted <- names(c(weather_station[[1]], weather_station[[2]], weather_station[[3]]))
  parameters <- as.character(unlist(list(...)))
  empty <- parameters[!parameters %in% unlisted]
  if(length(empty)>1){
    stop(paste(empty, collapse = ", "), " are not available in the weather_station object.\n",
         "Please set the needed parameters.")
  } else if(length(empty)>0){
    stop(paste(empty, collapse = ", "), " is not available in the weather_station object.\n",
         "Please set the needed parameter.")
  }
}

#' Create a data.frame from a weather station object
#'
#' Create a data.frame from a weather station object, that contains weather station measurements.
#'
#' @param x Object of class weather_station.
#' @param reduced TRUE, to only output the most important columns.
#' @param unit TRUE, to generate longer column labels with units.
#' @param ... Not used.
#'
#' @return data.frame
#' The columns of the data frame depend on `reduced` and `unit`.
#' If `reduced = F`, the data.frame contains all coloumns of the `measurements` list in `weather_station`.
#' If `reduced = T`, the data.frame contains: "datetime", "t1", "t2", "v1", "v2", "p1", "p2", "hum1", "hum2", "soil_flux", "sw_in", "sw_out", "lw_in", "lw_out", "sw_bal", "lw_bal", "rad_bal", "stability", "sensible_priestley_taylor", "latent_priestley_taylor","sensible_bowen", "latent_bowen","sensible_monin", "latent_monin","latent_penman"
#' If `unit = T`, the column names are replaced by more detailed names, containing the respective uits.
#' @export
#'
#' @examples
#' \dontrun{
#' #create a weather_station object
#' test_station <- build_weather_station(lat = 50.840503,
#'                                      lon = 8.6833,
#'                                      elev = 270,
#'                                      surface_type = "Meadow",
#'                                      obs_height = 0.3, # obstacle height
#'                                      z1 = 2, # measurement heights
#'                                      z2 = 10,
#'                                      datetime = ws$datetime,
#'                                      t1 = ws$t1, # temperature
#'                                      t2 = ws$t2,
#'                                      v1 = ws$v1, # windspeed
#'                                      v2 = ws$v2,
#'                                      hum1 = ws$hum1, # humidity
#'                                      hum2 = ws$hum2,
#'                                      sw_in = ws$rad_sw_in, # shortwave radiation
#'                                      sw_out = ws$rad_sw_out,
#'                                      lw_in = ws$rad_lw_in, # longwave radiation
#'                                      lw_out = ws$rad_lw_out,
#'                                      soil_flux = ws$heatflux_soil)
#'
#' #add turbulent fluxes to the object
#' station_turbulent <- turb_flux_calc(test_station)
#'
#' #create a data.frame which contains the same coloumns as the weather_station object
#' normal <- as.data.frame(station_turbulent)
#'
#' #create a reduced data.frame
#' reduced <- as.data.frame(station_turbulent, reduced = T)
#'
#' #create a reduced data.frame with detailed units
#' unit <- as.data.frame(station_turbulent, reduced = T, unit = T)
#' }
#'
as.data.frame.weather_station <- function(x, ...,
                                          reduced = F, unit = F){

  out <- as.data.frame(x$measurements)

  # Define important columns
  important <- c("datetime", "t1", "t2", "v1", "v2", "p1", "p2", "hum1", "hum2", "soil_flux",
                 "sw_in", "sw_out", "lw_in", "lw_out", "sw_bal", "lw_bal", "rad_bal", "stability",
                 "sensible_priestley_taylor", "latent_priestley_taylor",
                 "sensible_bowen", "latent_bowen",
                 "sensible_monin", "latent_monin",
                 "latent_penman")

  if(reduced){
    out <- out[,important[important %in% colnames(out)]]
  }

  if(unit){
    replacement <- c("datetime",
                      "temperature_lower[degC]",
                      "temperature_upper[degC]",
                      "wind_speed_lower[m/s]",
                      "wind_speed_upper[m/s]",
                      "pressure_lower[hPa]",
                      "pressure_upper[hPa]",
                      "humidity_lower[%]",
                      "humidity_upper[%]",
                      "soil_flux[W/m^2]",
                      "shortwave_radiation_in[W/m^2]",
                      "shortwave_radiation_out[W/m^2]",
                      "longwave_radiation_in[W/m^2]",
                      "longwave_radiation_out[W/m^2]",
                      "shortwave_radiation_balance[W/m^2]",
                      "longwave_radiation_balance[W/m^2]",
                      "total_radiation_balance[W/m^2]",
                      "atmospheric_stability",
                      "sensible_heat[W/m^2]_Priestly-Taylor",
                      "latent_heat[W/m^2]_Priestly-Taylor",
                      "sensible_heat[W/m^2]_Bowen",
                      "latent_heat[W/m^2]_Bowen",
                      "sensible_heat[W/m^2]_Monin",
                      "latent_heat[W/m^2]_Monin",
                      "latent_heat[W/m^2]_Penman"
    )

    for(i in seq_along(important)){
      names(out)[names(out) == important[i]] <- replacement[i]
    }

  }

  return(out)

}
