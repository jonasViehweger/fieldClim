#' Weather Station
#'
#' Creates a list of class "weather_station, that contains all data regarding the
#' weather station, its location and its measurements.
#'
#' Parameters with preset NULL can be estimated using calculations. However some additional
#' variables need to be passed for the estimation of some parameters.
#' For usage examples see the examples below.
#'
#' If p1 and p2 are NULL, they will get estimated using the elevation and air temperature.
#'
#' If sw_in is NULL, it will get estimated using TOA radiation and average
#' atmospheric transmittance (see [fieldClim::rad_sw_in]).
#' By setting slope, sky_view and exposition, sw_in will be topographically corrected
#' (see [fieldClim::rad_sw_in_topo]).
#'
#' If sw_out is NULL, albedo needs to be set (see [fieldClim::rad_sw_out]).
#'
#' If lw_in is NULL, it will get estimated using the air temperature and pressure
#' (see [fieldClim::rad_lw_in]).
#' By setting sky_view, lw_in will be topographically corrected
#' (see [fieldClim::rad_lw_in_topo]).
#'
#' If lw_out is NULL, t_surface needs to be set (see [fieldClim::rad_lw_out]).
#'
#' If soil_flux is NULL, ts1, ts2, depth1, depth2, moisture and texture need to be set.
#' (see [fieldClim::soil_heat_flux] and [fieldClim::soil_thermal_cond]).
#'
#' @param lat Latitude of location. Preset: 50.840503 (climate station caldern).
#' @param lon Longitude of location. Preset: 8.683300 (climate station caldern).
#' @param elev Elevation of location above sea level in m. Preset: 270 m (climate station caldern).
#' @param surface_type Surface Type. Form: Character string. One of: "Wiese", "Acker", "Gruenflaeche", "Strasse", "Landwirtschaft", "Siedlung", "Nadelwald", "Laubwald", "Mischwald", "Stadt". Preset: "Wiese.
#' @param obs_height Height of vegetation in m. Preset: 0.3.
#' @param z1 Lower measurement height in m. Preset: 2m.
#' @param z2 Upper measurement height in m. Preset: 2m.
#' @param datetime Name of datetime-coloumn in data.
#' Form: POSIX-Object (See [base::as.POSIXlt] and [base::strptime] for conversion.)
#' @param t1 Vector containing lower temperature data in degrees C.
#' @param t2 Vector containing upper temperature data in degrees C.
#' @param v1 Vector containing lower wind speed data in m/s.
#' @param v2 Vector containing upper wind speed data in m/s.
#' @param hum1 Vector containing lower humidity data in %.
#' @param hum2 Vector containing upper humidity data in %.
#' @param p1 Vector containing lower pressure data in hPa.
#' @param p2 Vector containing upper pressure data in hPa.
#' @param sw_in Vector containing incoming shortwave radiation in W/m^2.
#' @param sw_out Vector containing outgoing shortwave radiation in W/m^2.
#' @param lw_in Vector containing incoming longwave radiation in W/m^2.
#' @param lw_out Vector containing outgoing shortwave radiation in W/m^2.
#' @param soil_flux Vector containing soil flux in W/m^2.
#' @param ... Additional parameters, see details for usage.
#'
#' @return List of class "weather_station", that contains:
#' 1) list of location properties
#' 2) list of weather station properties
#' 3) list of measurements, which will contain NULLs if they were not defined in the input
#' @export
#'
#' @examples
#' \dontrun{
#' # Standard parameters
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
#' # Specify pressure
#' test_station <- build_weather_station(lat = 50.840503, lon = 8.6833, elev = 270,
#'                                      surface_type = "Meadow", obs_height = 0.3,
#'                                      z1 = 2, z2 = 10, datetime = ws$datetime,
#'                                      t1 = ws$t1, t2 = ws$t2, v1 = ws$v1, v2 = ws$v2,
#'                                      hum1 = ws$hum1, hum2 = ws$hum2,
#'                                      sw_in = ws$rad_sw_in,
#'                                      sw_out = ws$rad_sw_out,
#'                                      lw_in = ws$rad_lw_in,
#'                                      lw_out = ws$rad_lw_out,
#'                                      soil_flux = ws$heatflux_soil,
#'                                      # ADDED PRESSURE
#'                                      p1 = ws$p1,
#'                                      p2 = ws$p2)
#'
#' # Alternative calculation of soil flux
#' test_station <- build_weather_station(lat = 50.840503, lon = 8.6833, elev = 270,
#'                                      surface_type = "Meadow", obs_height = 0.3,
#'                                      z1 = 2, z2 = 10, datetime = ws$datetime,
#'                                      t1 = ws$t1, t2 = ws$t2, v1 = ws$v1, v2 = ws$v2,
#'                                      hum1 = ws$hum1, hum2 = ws$hum2,
#'                                      sw_in = ws$rad_sw_in,
#'                                      sw_out = ws$rad_sw_out,
#'                                      lw_in = ws$rad_lw_in,
#'                                      lw_out = ws$rad_lw_out,
#'                                      # Alternative Soil flux:
#'                                      depth1 = 0,
#'                                      depth2 = 0.3,
#'                                      ts1 = ws$t_surface,
#'                                      ts2 = ws$ts1,
#'                                      moisture = ws$water_vol_soil,
#'                                      texture = "clay")
#
#' # Alternative shortwave
#' test_station <- build_weather_station(lat = 50.840503, lon = 8.6833, elev = 270,
#'                                      surface_type = "Meadow", obs_height = 0.3,
#'                                      z1 = 2, z2 = 10, datetime = ws$datetime,
#'                                      t1 = ws$t1, t2 = ws$t2, v1 = ws$v1, v2 = ws$v2,
#'                                      hum1 = ws$hum1, hum2 = ws$hum2,
#'                                      lw_in = ws$rad_lw_in,
#'                                      lw_out = ws$rad_lw_out,
#'                                      soil_flux = ws$heatflux_soil,
#'                                      # Alternative shortwave radiation:
#'                                      albedo = 0.3,
#'                                      # Topographic correction
#'                                      slope = 10, # In degrees
#'                                      exposition = 20, # North = 0, South = 180
#'                                      sky_view = 0.82 # Sky view factor (0-1)
#' )
#'
#' # Alternative longwave
#' test_station <- build_weather_station(lat = 50.840503, lon = 8.6833, elev = 270,
#'                                      surface_type = "Meadow", obs_height = 0.3,
#'                                      z1 = 2, z2 = 10, datetime = ws$datetime,
#'                                      t1 = ws$t1, t2 = ws$t2, v1 = ws$v1, v2 = ws$v2,
#'                                      hum1 = ws$hum1, hum2 = ws$hum2,
#'                                      sw_in = ws$rad_sw_in,
#'                                      sw_out = ws$rad_sw_out,
#'                                      soil_flux = ws$heatflux_soil,
#'                                      # Alternative longwave radiation:
#'                                      t_surface = ws$t_surface,
#'                                      # Different emissivity:
#'                                      # lw_out = rad_lw_out(ws$t_surface, emissivity_surface = 0.92),
#'                                      # Topographic correction
#'                                      sky_view = 0.82 # Sky view factor (0-1)
#' )
#'
#' }
build_weather_station <-  function(lat,
                                   lon,
                                   elev,
                                   surface_type = "Meadow",
                                   obs_height = 0.3,
                                   z1,
                                   z2,
                                   datetime,
                                   t1,
                                   t2,
                                   v1,
                                   v2,
                                   hum1,
                                   hum2,
                                   p1 = NULL,
                                   p2 = NULL,
                                   sw_in = NULL,
                                   sw_out = NULL,
                                   lw_in = NULL,
                                   lw_out = NULL,
                                   soil_flux = NULL,
                                   ...){

  out_list <- list(location_properties = list(latitude = lat,
                                              longitude = lon,
                                              elevation = elev,
                                              surface_type = surface_type,
                                              obs_height = obs_height),
                   properties = list(z1 = z1,
                                     z2 = z2),
                   measurements = list(datetime = datetime,
                                       t1 = t1,
                                       t2 = t2,
                                       v1 = v1,
                                       v2 = v2,
                                       hum1 = hum1,
                                       hum2 = hum2,
                                       p1 = p1,
                                       p2 = p2,
                                       sw_in = sw_in,
                                       sw_out = sw_out,
                                       lw_in = lw_in,
                                       lw_out = lw_out,
                                       soil_flux = soil_flux))
  class(out_list) <- "weather_station"


  # Additional parameters
  add_location <- c("slope", "sky_view", "exposition", "texture", "albedo")
  add_heights <- c("depth1", "depth2")
  add_measurements <- c("ts1", "ts2", "moisture", "t_surface")

  args <- list(...)
  for(i in seq_along(args)) {
    # Add additional parameters to the right spot in the list
    name <- names(args)[i]
    value <-  args[[i]]

    if(name %in% add_location){
      out_list$location_properties[name] <- list(value)
    } else if(name %in% add_heights){
      out_list$properties[name] <- list(value)
    } else if(name %in% add_measurements){
      out_list$measurements[name] <- list(value)
    }

    assign(x = name, value = value)
  }

  # Check if all given measurements are numeric and datetime is POSIXt
  for(i in 2:length(out_list$measurements)){
    value <- out_list$measurements[[i]]
    if(!is.numeric(value)
       & !is.null(value)){
      name <- names(out_list$measurements)[i]
      warning(name, " is not numeric. Will attempt to convert to numeric.")
      out_list$measurements[[i]] <- as.numeric(value)
    }
  }

  if(!inherits(out_list$measurements$datetime, "POSIXt")){
    stop("datetime must be of class POSIXt.")
  }


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



  # ---- Shortwave ----
  sw_in_status <- is.null(sw_in)
  if(sw_in_status){
    out_list$measurements$sw_in <- rad_sw_in(out_list) # You could specify transmittance here
  }

  if(is.null(sw_out)){

    if(!exists("albedo", inherits = F)){
      stop("If sw_out is NULL, 'albedo' needs to be passed to build_weather_station.")
    }

    out_list$measurements$sw_out <- rad_sw_out(out_list) # You could specify transmittance here
  }

  if(sw_in_status
     & exists("sky_view", inherits = F)
     & exists("slope", inherits = F)
     & exists("exposition", inherits = F)){
    out_list$measurements$sw_in <- rad_sw_in_topo(out_list)
  }


  # ---- Longwave ----
  lw_in_status <- is.null(lw_in)
  if(lw_in_status){
    out_list$measurements$lw_in <- rad_lw_in(out_list)
  }

  if(is.null(lw_out)){

    if(!exists("t_surface", inherits = F)){
      stop("If lw_out is NULL, 't_surface' needs to be passed to build_weather_station.")
    }

    out_list$measurements$lw_out <- rad_lw_out(out_list)
  }

  if(exists("sky_view", inherits = F) & lw_in_status){
    out_list$measurements$lw_in <- rad_lw_in_topo(out_list)
  }


  # ---- Radiation balances ----

  #trans_total <- waiting for function being brought to new form
  out_list$measurements$sw_bal <- rad_sw_radiation_balance(out_list)

  # calculate lw_bal
  out_list$measurements$lw_bal <- out_list$measurements$lw_in - out_list$measurements$lw_out

  # calculate rad_bal
  out_list$measurements$rad_bal <- rad_bal_total(out_list)


  # ---- Soil Flux ----
  if(is.null(soil_flux)){
    if(!exists("texture", inherits = F)
       & !exists("depth1", inherits = F)
       & !exists("depth2", inherits = F)
       & !exists("ts1", inherits = F)
       & !exists("ts2", inherits = F)
       & !exists("moisture", inherits = F)){
      stop("If soil_flux is NULL, 'texture', 'depth1', 'depth2', 'ts1', 'ts2' and 'moisture'",
           "need to be passed to build weather_station.")
    }

    out_list$measurements$soil_flux <- soil_heat_flux(out_list)

  }

  # ---- Stability ----#
  out_list$measurements$stability <- turb_flux_stability(out_list)

  # check if all vectors have the same length and print a warning if not
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

  #check if z1 < z2
  if(z1 > z2){
    stop("z1 must be smaller than z2! Please check the order of your input variables.")
  }

  return(out_list)
}
