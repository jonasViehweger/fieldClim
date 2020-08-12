#' Climate Station Properties
#'
#' Creates a element, that contains all data regarding the climate station, its location and its measurements.
#'
#' @param lat Latitude of location. Preset: 50.840502777777788.683303333333333 (climate station caldern).
#' @param lon Longitude of location. Preset: 8.683303333333333 (climate station caldern).
#' @param elev Elevation of location above sea level in m. Preset: 270 m (climate station caldern).
#' @param surface_type Surface Type. Form: Character string. One of: "Wiese", "Acker", "Gruenflaeche", "Strasse", "Landwirtschaft", "Siedlung", "Nadelwald", "Laubwald", "Mischwald", "Stadt". Preset: "Wiese.
#' @param obs_height Height of vegetation in m. Preset: 0.3.
#' @param texture Texture of ground. Form: Character string. One of: "clay", "sand". Preset: "clay".
#' @param valley TRUE, if climate station is positioned in a valley. Form: Boolean. Preset: FALSE.
#' @param slope Slope of hillside in %. Form: Integer or Numeric. Preset: NULL.
#' @param z1 Lower measurement height in m. Preset: 2m.
#' @param z2 Upper measurement height in m. Preset: 2m.
#' @param depth1 Upper depth of measurment in m. Preset: NULL. Note: Only needed, if soil flux shall be calculated.
#' @param depth2 Lower depth of measurment in m. Preset: NULL. Note: Only needed, if soil flux shall be calculated.
#' @param datetime Name of datetime-coloumn in data. Form: Character string. NOTE: datetime needs to be converted POSIXlt-Format (see ?as.POSIXlt)
#' @param t1 Vector that contains lower temperature data. Form: Character string.
#' @param t2 Vector that contains upper temperature data. Form: Character string.
#' @param v1 Vector that contains lower wind speed data. Form: Character string.
#' @param v2 Vector that contains upper wind speed data. Form: Character string.
#' @param hum1 Vector that contains lower humidity data. Form: Character string.
#' @param hum2 Vector that contains upper humidity data. Form: Character string.
#' @param p1 #Vector that contains lower pressure data. Form: Character string. Preset: NULL. Note: If NULL, pressure will be calculated.
#' @param p2 #Vector that contains upper pressure data. Form: Character string. Preset: NULL. Note: If NULL, pressure will be calculated.
#' @param rad_bal Vector that contains total radiation balance. Form: Character string. Preset: NULL. Note: If NULL, rad_bal will be calculated (Albedo needed).
#' @param sw_bal Vector that contains shortwave radiation balance. Form: Character string. Preset: NULL. Note: If NULL, sw_bal will be calculated (Albedo needed).
#' @param lw_bal Vector that contains longwave radiation balance. Form: Character string. Preset: NULL. Note: If NULL, lw_bal will be calculated (Albedo needed).
#' @param albedo Vector that contains albedo. Form: Character string. Preset: NULL. Note: Only needed, if radiation balances shall be calculated.
#' @param soil_flux Vector that contains soil flux. Form: Character string. Preset: NULL. Note: If NULL, soil_flux will be calculated.
#' @param ts1 Vector that contains upper ground temperature data. Form: Character string. Preset: NULL. Note: Only needed, if soil flux shall be calculated.
#' @param ts2 Vector that contains lower ground temperature data. Form: Character string. Preset: NULL. Note: Only needed, if soil flux shall be calculated.
#' @param moisture Vector that ground moisture data. Form: Character string. Preset: NULL. Note: Only needed, if soil flux shall be calculated.
#'
#' @return List containing:
#' 1) list of climate station location properties
#' 2) list of climate station properties
#' 3) list of climate station measurements, which will conatin NAs if they were not defined in the input
#' @export
#'
#' @examples
climate_station <-  function(lat = 50.84050277777778, #climate station caldern
                             lon = 8.683303333333333, #climate station caldern
                             elev = 270, #climate station caldern
                             surface_type = "Wiese",  #climate station caldern
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
                             p2 = NULL
                             rad_bal = NULL, #if NULL -> will be calculated, albedo needed
                             sw_bal = NULL, #if NULL -> will be calculated, albedo needed
                             lw_bal = NULL, #if NULL -> tough luck, won't be calculated
                             albedo = NULL, #needed, if radiation balance is unknown an shall be calulated
                             soil_flux = NULL,
                             ts1 = NULL,
                             ts2 = NULL,
                             moisture = NULL #needed when soil_flux unknown
){
  out_list <- list(climate_station_location_properties = list(latitude = lat,
                                                              longitude = lon,
                                                              elevation = elev,
                                                              surface_type = surface_type,
                                                              obs_height = obs_height,
                                                              texture = texture,
                                                              valley = valley,
                                                              slope = slope),
                   climate_station_properties = list(z1 = z1,
                                                     z2 = z2,
                                                     depth1 = depth1,
                                                     depth2 = depth2,),
                   climate_station_measurements = list(datetime = datetime,
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




  out_list$climate_station_measurements <- lapply(out_list$climate_station_measurements, function(i){
    if(length(i)==0 | length(i)==1){
      return(rep(NA, length(datetime)))
    }
    else(return(i))
  })

  # brings all vectors in "out_list$climate_station_measurements" to the length of the "datetime" vector by either filling it up with NA or removing tailing
  # entries of vectors, that are longer than "datetime"
  out_list$climate_station_measurements <- lapply(out_list$climate_station_measurements, "length<-", length(out_list$climate_station_measurements$datetime))

  return(out_list)
}
