#' Example data of a weather station located in Caldern, Hessen, Germany.
#'
#' A dataset containing different weather station measurements over the course of one day (2018-07-28).
#' The weather station is located near Caldern, Hessen, Germany.
#' Latitude: 50.840503
#' Longitude: 8.683300
#' Meters above sea level (elevation): 270
#' Heights of measurements: 2m (lower level) & 10m (upper level)
#' Surface type: "Wiese"
#' Ground texture: "clay"
#'
#' @format A data frame with 288 rows and 20 variables:
#' \describe{
#'   \item{datetime}{Date and time of measurement in POSIXt-format.}
#'   \item{t1}{Temperature in °C at 2m height (lower level).}
#'   \item{hum1}{Humidity in % at 2m height (lower level).}
#'   \item{t2}{Temperature in °C at 10m height (upper level).}
#'   \item{hum2}{Humidity in % at 10m height (upper level).}
#'   \item{rad_sw_in}{Incoming shortwave radiation in W/m^2.}
#'   \item{rad_sw_out}{Outgoing shortwave radiation in W/m^2.}
#'   \item{rad_lw_in}{Incoming longwave radiation in W/m^2.}
#'   \item{rad_lw_out}{Outgoing longwave radiation in W/m^2.}
#'   \item{rad_sw_bal}{Shortwave radiation balance in W/m^2.}
#'   \item{rad_lw_bal}{Longwave radiation balance in W/m^2.}
#'   \item{albedo}{Albedo of ground.}
#'   \item{rad_bal}{Total radiation balance in W/m^2.}
#'   \item{water_vol_soil}{Moisture of ground in %.}
#'   \item{t_surface}{Surface temperature in °C.}
#'   \item{ts1}{Temperature Soil, 30 cm depth.}
#'   \item{heatflux_soil}{Soil heat flux in W/m^2.}
#'   \item{v1}{Wind velocity in m/s at 2m height (lower level).}
#'   \item{v2}{Wind velocity in m/s at 10m height (upper level).}
#'   \item{p1}{Air pressure in hPa (lower level).}
#'   \item{p2}{Air pressure in hPa (upper level).}
#' }
#' @source Provided by Prof. Dr. Jörg Bendix, Laboratory of Climatology and Remote Sensing, Philipps-University of Marburg.
"weather_station_example_data"

#' Emissivity and roughness lenght for different surfaces.
#'
#' A dataset containing values for emissivity and roughness length for different types of surface
#'
#' @format A data frame with 10 rows and 3 variables:
#' \describe{
#'   \item{surface_type}{Type of surface}
#'   \item{emissivity}{Emissivity of the chosen surface}
#'   \item{roughness_length}{Rougness lenght of surface in }
#' }
"surface_properties"
