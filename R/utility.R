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
#'
#' @return data.frame containing
#' "datetime",
#' "temperature_lower[C°]",
#' "temperature_upper[C°]",
#' "wind_speed_lower[m/s]",
#' "wind_speed_upper[m/s]",
#' "pressure_lower[hPa]",
#' "pressure_upper[hPa]",
#' "humidity_lower[%]",
#' "humidity_upper[%]",
#' "soil_flux[W/m²]",
#' "shortwave_radiation_incoming[W/m²]",
#' "shortwave_radiation_outgoing[W/m²]",
#' "longwave_radiation_incoming[W/m²]",
#' "longwave_radiation_outgoing[W/m²]",
#' "shortwave_radiation_balance[W/m²]",
#' "longwave_radiation_balance[W/m²]",
#' "total_radiation_balance[W/m²]",
#' "atmospheric_stability",
#' "sensible_heat[W/m^2]_Priestly-Taylor",
#' "latent_heat[W/m^2]_Priestly-Taylor",
#' "sensible_heat[W/m^2]_Bowen",
#' "latent_heat[W/m^2]_Bowen",
#' "sensible_heat[W/m^2]_Monin",
#' "latent_heat[W/m^2]_Monin",
#' "latent_heat[W/m^2]_Penman"
#' @export
#'
#' @examples
as.data.frame <- function(weather_station){

  out <- data.frame(datetime = weather_station$measuerements$datetime,
                    t1 = weather_station$measuerements$t1,
                    t2 = weather_station$measuerements$t2,
                    v1 = weather_station$measuerements$v1,
                    v2 = weather_station$measuerements$v2,
                    p1 = weather_station$measuerements$p1,
                    p2 = weather_station$measuerements$p2,
                    hum1 = weather_station$measuerements$hum1,
                    hum2 = weather_station$measuerements$hum2,
                    soil_flux = weather_station$measuerements$soil_flux,
                    sw_in = weather_station$measuerements$sw_in,
                    sw_out = weather_station$measuerements$sw_out,
                    lw_in = weather_station$measuerements$lw_in,
                    lw_out = weather_station$measuerements$lw_out,
                    sw_radiation_balance = weather_station$measuerements$sw_bal,
                    lw_radiation_balance = weather_station$measuerements$lw_bal,
                    total_radiation_balance = weather_station$measuerements$rad_bal,
                    stability_of_atmosphere = weather_station$measuerements$stability,
                    sensible_heat_priestly_taylor = weather_station$measuerements$sensible_heat_Priestley_Taylor,
                    latent_heat_priestly_taylor = weather_station$measuerements$latent_heat_Priestley_Taylor,
                    sensible_heat_bowen = weather_station$measuerements$sensible_heat_Bowen,
                    latent_heat_bowen = weather_station$measuerements$latent_heat_Bowen,
                    sensible_heat_monin = weather_station$measuerements$sensible_heat_Monin,
                    latent_heat_monin = weather_station$measuerements$latent_heat_Monin,
                    latent_heat_penman_weather_station$measuerements$latent_heat_Penman
  )

  colnames(out) <- c("datetime",
                     "temperature_lower[C°]",
                     "temperature_upper[C°]",
                     "wind_speed_lower[m/s]",
                     "wind_speed_upper[m/s]",
                     "pressure_lower[hPa]",
                     "pressure_upper[hPa]",
                     "humidity_lower[%]",
                     "humidity_upper[%]",
                     "soil_flux[W/m²]",
                     "shortwave_radiation_in[W/m²]",
                     "shortwave_radiation_out[W/m²]",
                     "longwave_radiation_in[W/m²]",
                     "longwave_radiation_out[W/m²]",
                     "shortwave_radiation_balance[W/m²]",
                     "longwave_radiation_balance[W/m²]",
                     "atmospheric_stability",
                     "total_radiation_balance[W/m²]",
                     "sensible_heat[W/m^2]_Priestly-Taylor",
                     "latent_heat[W/m^2]_Priestly-Taylor",
                     "sensible_heat[W/m^2]_Bowen",
                     "latent_heat[W/m^2]_Bowen",
                     "sensible_heat[W/m^2]_Monin",
                     "latent_heat[W/m^2]_Monin",
                     "latent_heat[W/m^2]_Penman")

  return(out)

}
