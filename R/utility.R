#' Check Availability
#'
#' Checks availability of passed properties in the weather station object.
#' If property is NULL, aborts with error.
#'
#' @param weather_station Object of class weather_station.
#' @param ... Strings of properties to check.
#'
#' @return
check_availability <- function(weather_station, ...){
  unlisted <- c(weather_station[[1]], weather_station[[2]], weather_station[[3]])
  parameters <- unlisted[c(...)]
  empty <- names(which(sapply(parameters, is.null)))
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
#' @param weather_station Object of class weather_station.
#' @param reduced TRUE, to only output the most important columns.
#' @param units TRUE, to generate longer column labels with units.
#'
#' @return data.frame
#' @export
#'
as.data.frame.weather_station <- function(weather_station, reduced = F, units = F){

  out <- as.data.frame(weather_station$measurements)

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

  if(units){
    replacement <- c("datetime",
                      "temperature_lower[°C]",
                      "temperature_upper[°C]",
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
