#' Weather Station
#'
#' Creates a list of class "weather_station, that contains all data regarding the weather station, its location and its measurements.
#'
#' @param lat Latitude of location. Preset: 50.84050277777778 (climate station caldern).
#' @param lon Longitude of location. Preset: 8.683303333333333 (climate station caldern).
#' @param elev Elevation of location above sea level in m. Preset: 270 m (climate station caldern).
#' @param surface_type Surface Type. Form: Character string. One of: "Wiese", "Acker", "Gruenflaeche", "Strasse", "Landwirtschaft", "Siedlung", "Nadelwald", "Laubwald", "Mischwald", "Stadt". Preset: "Wiese.
#' @param obs_height Height of vegetation in m. Preset: 0.3.
#' @param texture Texture of ground. Form: Character string. One of: "clay", "sand". Preset: "clay".
#' @param valley TRUE, if climate station is positioned in a valley. Form: logical. Preset: FALSE.
#' @param slope Slope of hillside in %. Form: Numeric. Preset: NULL.
#' @param z1 Lower measurement height in m. Preset: 2m.
#' @param z2 Upper measurement height in m. Preset: 2m.
#' @param depth1 Upper depth of soil measurment in m. Preset: NULL. Note: Only needed, if soil flux shall be calculated.
#' @param depth2 Lower depth of soil measurment in m. Preset: NULL. Note: Only needed, if soil flux shall be calculated.
#' @param datetime Name of datetime-coloumn in data.
#' Form: POSIX-Object (See [base::as.POSIXlt] and [base::strptime] for conversion.)
#' @param t1 Vector containing lower temperature data in degrees C.
#' @param t2 Vector containing upper temperature data in degrees C.
#' @param v1 Vector containing lower wind speed data in m/s.
#' @param v2 Vector containing upper wind speed data in m/s.
#' @param hum1 Vector containing lower humidity data in %.
#' @param hum2 Vector containing upper humidity data in %.
#' @param p1 Vector containing lower pressure data in hPa.
#' Preset: NULL. Note: If NULL, pressure will be estimated.
#' @param p2 Vector containing upper pressure data in hPa.
#' Preset: NULL. Note: If NULL, pressure will be calculated.
#' @param rad_bal Vector containing total radiation balance in W/m^2.
#' Preset: NULL. Note: If NULL, rad_bal will be calculated (Albedo needed).
#' @param sw_bal Vector containing shortwave radiation balance in W/m^2.
#' Preset: NULL. Note: If NULL, sw_bal will be calculated (Albedo needed).
#' @param lw_bal Vector containing longwave radiation balance in W/m^2.
#' Preset: NULL. Note: If NULL, lw_bal will be calculated (Albedo needed).
#' @param albedo Vector containing albedo.
#' Preset: NULL. Note: Only needed, if radiation balances shall be calculated.
#' @param soil_flux Vector containing soil flux in W/m^2.
#' Preset: NULL. Note: If NULL, soil_flux will be calculated.
#' @param ts1 Vector containing upper ground temperature data in degrees C.
#' Preset: NULL. Note: Only needed, if soil flux shall be calculated.
#' @param ts2 Vector containing lower ground temperature data in degrees C.
#' Preset: NULL. Note: Only needed, if soil flux shall be calculated.
#' @param moisture Vector containing ground moisture data in Vol-%.
#' Preset: NULL. Note: Only needed, if soil flux shall be calculated.
#'
#' @return List of class "weather_station", that contains:
#' 1) list of location properties
#' 2) list of weather station properties
#' 3) list of measurements, which will conatin NAs if they were not defined in the input
#' @export
#'
#' @examples
build_weather_station <-  function(lat = 50.84050277777778, #weather station caldern
                                   lon = 8.683303333333333, #weather station caldern
                                   elev = 270, #weather station caldern
                                   surface_type = "Wiese",  #weather station caldern
                                   obs_height = 0.3,
                                   texture = "clay", #needed when soil_flux unknown
                                   valley = F,
                                   slope = NULL, #if not NULL, the rad_bal with topography will be calculated
                                   z1,
                                   z2,
                                   depth1 = NULL, #needed when soil_flux unknown
                                   depth2 = NULL, #needed when soil_flux unknown
                                   datetime,
                                   t1,
                                   t2,
                                   v1,
                                   v2,
                                   hum1,
                                   hum2,
                                   p1 = NULL,
                                   p2 = NULL,
                                   rad_bal = NULL, #if NULL -> will be calculated, albedo needed
                                   sw_bal = NULL, #if NULL -> will be calculated, albedo needed
                                   lw_bal = NULL, #if NULL -> tough luck, won't be calculated
                                   albedo = NULL, #needed, if radiation balance is unknown an shall be calulated
                                   soil_flux = NULL,
                                   ts1 = NULL,
                                   ts2 = NULL,
                                   moisture = NULL){ #needed when soil_flux unknown
  out_list <- list(location_properties = list(latitude = lat,
                                              longitude = lon,
                                              elevation = elev,
                                              surface_type = surface_type,
                                              obs_height = obs_height,
                                              texture = texture,
                                              valley = valley,
                                              slope = slope),
                   properties = list(z1 = z1,
                                     z2 = z2,
                                     depth1 = depth1,
                                     depth2 = depth2),
                   measurements = list(datetime = datetime,
                                       t1 = t1,
                                       t2 = t2,
                                       v1 = v1,
                                       v2 = v2,
                                       hum1 = hum1,
                                       hum2 = hum2,
                                       p1 = p1,
                                       p2 = p2,
                                       rad_bal = rad_bal,
                                       sw_bal = sw_bal,
                                       lw_bal = lw_bal,
                                       albedo = albedo,
                                       soil_flux = soil_flux,
                                       ts1 = ts1,
                                       ts2 = ts2,
                                       moisture = moisture))
  class(out_list) <- "weather_station"


  # If there is an actual pressure measurement use that for both heights
  # instead of estimating the other height
  if(!is.null(p1) & is.null(p2)){
    p2 <- p1
  }
  if(is.null(p1) & !is.null(p2)){
    p1 <- p2
  }

  # calculate pressure
  if(is.null(p1)){
    out_list$measurements$p1 <- pres_p(out_list, "lower")
  }

  if(is.null(p2)){
    out_list$measurements$p2 <- pres_p(out_list, "upper")
  }

  # calculate rad_bal -> doing this, when functions are ready

  # calculate sw_bal -> doing this, when functions are ready

  # calculate lw_bal -> doing this, when functions are ready

  # check if all vectors have the same lenght and print a warning if not
  length_condition <- lengths(out_list$measurements[2:length(out_list$measurements)]) != length(out_list$measurements$datetime)
  null_check <- lengths(out_list$measurements[2:length(out_list$measurements)])>0
  if(any(length_condition & null_check)){
    wrong <- names(which(length_condition & null_check))
    if(length(wrong) == 1){
      stop(paste(wrong, collapse = ", "), " is not the same length as datetime!\n",
           "Please make sure, that all input-vectors have the same length")
    } else {
      stop(paste(wrong, collapse = ", "), " are not the same length as datetime!\n",
           "Please make sure, that all input-vectors have the same length")
    }
  }

  return(out_list)
}

#example data
# lat = 50.84050277777778 #weather station caldern
# lon = 8.683303333333333 #weather station caldern
# elev = 270 #weather station caldern
# surface_type = "Wiese"  #weather station caldern
# obs_height = 0.3
# texture = "clay" #needed when soil_flux unknown
# valley = F
# slope = NULL #if not NULL, the rad_bal with topography will be calculated
# z1 = 2
# z2 = 10
# depth1 = NULL #needed when soil_flux unknown
# depth2 = NULL #needed when soil_flux unknown
# datetime = c(1,2,3,4,5,6,7,8)
# t1 = c(1,2,3,4,5)
# t2 = c(4,5,7,78,2)
# v1 = c(1,23,4,6,23)
# v2 = c(2,4,6,4,2,4,6)
# hum1 = c(2,4,6,4,2,4,6)
# hum2 = c(2,4,6,4,2,4,6)
# p1 = NULL
# p2 = NULL
# rad_bal = NULL #if NULL -> will be calculated, albedo needed
# sw_bal = NULL#if NULL -> will be calculated, albedo needed
# lw_bal = NULL#if NULL -> tough luck, won't be calculated
# albedo = NULL #needed, if radiation balance is unknown an shall be calulated
# soil_flux = NULL
# ts1 = NULL
# ts2 = NULL
# moisture = NULL
