#' Emissivity of the atmosphere
#'
#' Calculation of the emissivity of the atmosphere.
#'
#'
#' @return Emissivity of the atmosphere (0-1).
#' @export
#'
rad_emissivity_air <- function (...) {
  UseMethod("rad_emissivity_air")
}

#' @rdname rad_emissivity_air
#' @method rad_emissivity_air numeric
#' @param t Air temperature in degrees C.
#' @param elev Meters above sea level
#' @param p OPTIONAL. Air pressure in hPa.
#' If not available, will be calculated from elev and air temperature.
rad_emissivity_air.numeric <- function(t, elev, p = NULL){
  if(is.null(p)) p <- pres_p(elev, t)
  svp <- hum_sat_vapor_pres(t)
  t_over <- t*(0.0065*elev)
  eat <- ((1.24*svp/t)**1/7)*(p/1013.25)
  return(eat)
}

#' @rdname rad_emissivity_air
#' @method rad_emissivity_air weather_station
#' @export
#' @param weather_station Object of class weather_station.
#' @param height Height of measurement. "lower" or "upper".
rad_emissivity_air.weather_station <- function(weather_station, height) {
  check_availability(weather_station, "t1", "t2", "elevation", "p1", "p2")
  if(!height %in% c("upper", "lower")){
    stop("'height' must be either 'lower' or 'upper'.")
  }

  height_num <- which(height == c("lower", "upper"))
  t <- weather_station$measurements[[paste0("t", height_num)]]
  p <- weather_station$measurements[[paste0("p", height_num)]]
  elev <- weather_station$location_properties$elevation

  return(rad_emissivity_air(t, elev, p))
}



#' Longwave radiation of the surface
#'
#' Calculates emissions of a surface.
#'
#' If a weather_station object is given, the lower air temperature will be used
#' instead of the surface temperature.
#'
#'
#' @return Emissions in W/m^2.
#' @export
#'
rad_lw_surface <- function (...) {
  UseMethod("rad_lw_surface")
}

#' @rdname rad_lw_surface
#' @method rad_lw_surface numeric
#' @param t Surface temperature in degrees C.
#' @param emissivity_surface Emissivity of surface.
#' Default is emissivity for short grass.
rad_lw_surface.numeric <- function(t, emissivity_surface = 0.95){
  sigma <- 5.670374e-8
  return(emissivity_surface*sigma*(t+273.15)**4)
}

#' @rdname rad_lw_surface
#' @method rad_lw_surface weather_station
#' @export
#' @param weather_station Object of class weather_station.
#' @param emissivity_surface Emissivity of surface.
#' Default is emissivity for short grass.
rad_lw_surface.weather_station <- function(weather_station, emissivity_surface = 0.95) {
  check_availability(weather_station, "t1")

  t <- weather_station$measurements$t1

  return(rad_lw_surface(t, emissivity_surface))
}



#' Longwave radiation of the atmosphere
#'
#' Calculation of the longwave radiation of the atmosphere.
#'
#'
#' @return Atmospheric radiation in W/m^2.
#' @export
#'
rad_lw_atmospheric <- function (...) {
  UseMethod("rad_lw_atmospheric")
}

#' @rdname rad_lw_atmospheric
#' @method rad_lw_atmospheric numeric
#' @param emissivity_air Emissivity of the atmosphere (factor: 0-1)
#' @param t Air temperature in degrees C.
rad_lw_atmospheric.numeric <- function(emissivity_air, t){
  sigma <- 5.670374e-8
  gs <- emissivity_air*sigma*(t+273.15)**4
  return(gs)
}

#' @rdname rad_lw_atmospheric
#' @method rad_lw_atmospheric weather_station
#' @export
#' @param weather_station Object of class weather_station.
rad_lw_atmospheric.weather_station <- function(weather_station) {
  check_availability(weather_station, "t2")

  t <- weather_station$measurements$t2
  emissivity_air <- rad_emissivity_air(weather_station, "upper")

  return(rad_lw_atmospheric(emissivity_air, t))
}



#' Shortwave radiation at top of atmosphere
#'
#' Calculation of the shortwave radiation at the top of the atmosphere.
#'
#'
#' @return Shortwave radiation at top of atmosphere in W/m^2.
#' @export
#'
rad_sw_toa <- function (...) {
  UseMethod("rad_sw_toa")
}

#' @rdname rad_sw_toa
#' @method rad_sw_toa POSIXt
#' @param datetime Date and time as POSIX type.
#' See [base::as.POSIXlt()] and [base::strptime] for conversion.
#' @param lat Latitude of the place of the climate station in decimal degrees.
#' @param lon Longitude of the place of the climate station in decimal degrees.
rad_sw_toa.POSIXt <- function(datetime, lat, lon){
  if(!inherits(datetime, "POSIXt")){
    stop("datetime has to be of class POSIXt.")
  }
  sol_const <- 1367
  sol_eccentricity <- sol_eccentricity(datetime)
  sol_elevation <- sol_elevation(datetime, lat, lon)
  rad_sw_toa <- sol_const*sol_eccentricity*sin(sol_elevation*(pi/180))
  return(rad_sw_toa)
}

#' @rdname rad_sw_toa
#' @method rad_sw_toa weather_station
#' @export
#' @param weather_station Object of class weather_station.
rad_sw_toa.weather_station <- function(weather_station) {
  check_availability(weather_station, "datetime", "latitude", "longitude")

  datetime <- weather_station$measurements$datetime
  lat <- weather_station$location_properties$latitude
  lon <- weather_station$location_properties$longitude

  return(rad_sw_toa(datetime, lat, lon))
}



#' Shortwave radiation onto a horizontal area on the ground
#'
#' Calculation of the shortwave radiation onto a horizontal area on the ground.
#'
#'
#' @return Shortwave radiation on the ground onto a horizontal area in W/m^2.
#' @export
#'
rad_sw_ground_horizontal <- function (...) {
  UseMethod("rad_sw_ground_horizontal")
}

#' @rdname rad_sw_ground_horizontal
#' @method rad_sw_ground_horizontal numeric
#' @param rad_sw_toa Shortwave radiation at top of atmosphere in W/m^2.
#' @param trans_total Total transmittance of the atmosphere (0-1).
rad_sw_ground_horizontal.numeric <- function(rad_sw_toa, trans_total){
  rad_sw_ground_horizontal <- rad_sw_toa*trans_total
  return(rad_sw_ground_horizontal)
}

#' @rdname rad_sw_ground_horizontal
#' @method rad_sw_ground_horizontal weather_station
#' @export
#' @param weather_station Object of class weather_station.
#' @param trans_total Total transmittance of the atmosphere.
#' @param oz OPTIONAL. Needed if trans_total = NULL. Columnar ozone in cm.
#' Default is average global value.
#' @param vis OPTIONAL. Needed if trans_total = NULL. Meteorological visibility in km.
#' Default is the visibility on a clear day.
rad_sw_ground_horizontal.weather_station <- function(weather_station,
                                                     trans_total = NULL, oz = 0.35, vis = 30) {

  rad_sw_toa <- rad_sw_toa(weather_station)
  if(is.null(trans_total)){
    trans_total <- trans_total(weather_station, oz, vis)
  }

  return(rad_sw_ground_horizontal(rad_sw_toa, trans_total))
}



#' Reflected shortwave ratdiation
#'
#' Calculation of the reflected shortwave radiation.
#'
#'
#' @return Reflected shortwave ratdiation in W/m^2.
#' @export
#'
rad_sw_reflected <- function (...) {
  UseMethod("rad_sw_reflected")
}

#' @rdname rad_sw_reflected
#' @method rad_sw_reflected numeric
#' @param rad_sw_ground_horizontal Shortwave radiation on the ground onto a horizontal area in W/m^2.
#' @param albedo Albedo of the surface.
rad_sw_reflected.numeric <- function(rad_sw_ground_horizontal, albedo){
  rad_sw_reflected <- rad_sw_ground_horizontal*albedo
  return(rad_sw_reflected)
}

#' @rdname rad_sw_reflected
#' @method rad_sw_reflected weather_station
#' @export
#' @param weather_station Object of class weather_station.
#' @param trans_total Total transmittance of the atmosphere.
#' @param oz OPTIONAL. Needed if trans_total = NULL. Columnar ozone in cm.
#' Default is average global value.
#' @param vis OPTIONAL. Needed if trans_total = NULL. Meteorological visibility in km.
#' Default is the visibility on a clear day.
rad_sw_reflected.weather_station <- function(weather_station,
                                             trans_total = NULL, oz = 0.35, vis = 30) {
  check_availability(weather_station, "albedo")

  rad_sw_ground_horizontal <- rad_sw_ground_horizontal(weather_station,
                                                       trans_total, oz, vis)
  albedo <- weather_station$measurements$albedo

  return(rad_sw_reflected(rad_sw_ground_horizontal, albedo))
}



#' Shortwave radiation balance
#'
#' Calculation of the shortwave radiation balance.
#'
#'
#' @return Shortwave radiation balance in W/m^2.
#' @export
#'
rad_sw_radiation_balance <- function (...) {
  UseMethod("rad_sw_radiation_balance")
}

#' @rdname rad_sw_radiation_balance
#' @method rad_sw_radiation_balance numeric
#' @param rad_sw_ground_horizontal Shortwave radiation on the ground onto a horizontal area in W/m^2.
#' @param rad_sw_reflected Reflected shortwave ratdiation in W/m^2.
rad_sw_radiation_balance.numeric <- function(rad_sw_ground_horizontal, rad_sw_reflected){
  rad_sw_radiation_balance <- rad_sw_ground_horizontal-rad_sw_reflected
  return(rad_sw_radiation_balance)
}

#' @rdname rad_sw_radiation_balance
#' @method rad_sw_radiation_balance weather_station
#' @export
#' @param weather_station Object of class weather_station.
#' @param trans_total Total transmittance of the atmosphere.
#' @param oz OPTIONAL. Needed if trans_total = NULL. Columnar ozone in cm.
#' Default is average global value.
#' @param vis OPTIONAL. Needed if trans_total = NULL. Meteorological visibility in km.
#' Default is the visibility on a clear day.
rad_sw_radiation_balance.weather_station <- function(weather_station,
                                                     trans_total = NULL, oz = 0.35, vis = 30) {

  rad_sw_ground_horizontal <- rad_sw_ground_horizontal(weather_station,
                                                       trans_total, oz, vis)
  rad_sw_reflected <- rad_sw_reflected(weather_station,
                                       trans_total, oz, vis)

  return(rad_sw_radiation_balance(rad_sw_ground_horizontal, rad_sw_reflected))
}



#' Shortwave radiation balance in dependency of topography
#'
#' Calculate shortwave radiation balance in dependency of topography.
#'
#'
#' @return Shortwave radiation balance in dependency of topography in W/m^2.
#' @export
#'
rad_sw_balance_topography <- function (...) {
  UseMethod("rad_sw_balance_topography")
}

#' @rdname rad_sw_balance_topography
#' @method rad_sw_balance_topography numeric
#' @param slope Slope in degrees.
#' @param valley Is the climate station placed in a valley (TRUE/FALSE)?
#' @param sol_elevation Sun elevation in degrees.
#' @param sol_azimuth Sun azimuth in degrees.
#' @param exposition Exposition (North = 0, South = 180)
#' @param rad_sw_ground_horizontal Shortwave radiation on the ground onto a horizontal area in W/m^2.
#' @param albedo Albedo of surface.
rad_sw_balance_topography.numeric <- function(slope, valley = F,
                                        sol_elevation, sol_azimuth,
                                        exposition = 0,
                                        rad_sw_ground_horizontal, albedo,
                                        trans_total){
  sol_dir <- rad_sw_ground_horizontal*0.9751*trans_total
  sol_dif <- rad_sw_ground_horizontal-sol_dir
  terr_sky_view <- terr_sky_view(slope, valley)
  f <- (pi/180)
  if(slope > 0) {
    terrain_angle <- (cos(slope*f)*sin(sol_elevation*f)
                      + sin(slope*f)*cos(sol_elevation*f)*cos(sol_azimuth*f
                                                              -(exposition*f)))
    rad_sw_topo_direct <- (sol_dir/sin(sol_elevation*f))*terrain_angle
  }
  if(slope == 0) {
    rad_sw_topo_direct <- sol_dir
  }
  if(terr_sky_view < 1) {
    rad_sw_topo_diffuse <- sol_dif*terr_sky_view
  }
  if(terr_sky_view == 1) {
    rad_sw_topo_diffuse <- sol_dif
  }
  sol_ter <- (rad_sw_topo_direct + rad_sw_topo_diffuse) * albedo * (1-terr_sky_view)
  sol_bal_topo <- (rad_sw_topo_direct - rad_sw_topo_diffuse) + sol_ter
  return(sol_bal_topo)
}



#' Total radiation balance
#'
#' Calculate total radiation balance.
#'
#'
#' @return Total radiation balance in W/m^2.
#' @export
#'
rad_bal_total <- function (...) {
  UseMethod("rad_bal_total")
}

#' @rdname rad_bal_total
#' @method rad_bal_total numeric
#' @param rad_sw_radiation_balance Shortwave radiation balance in W/m^2.
#' @param rad_lw_surface Longave surface emissions in W/m^2.
#' @param rad_lw_atmospheric Atmospheric radiation in W/m^2.
rad_bal_total.numeric <-function(rad_sw_radiation_balance, rad_lw_surface, rad_lw_atmospheric){
  radbil <- rad_sw_radiation_balance - (rad_lw_surface-rad_lw_atmospheric)
  return(radbil)
}



#' Total radiation balance with topography
#'
#' Calculates the total radiation balance with topography.
#'
#'
#' @return Total radiation balance with topography in W/m^2.
#' @export
#'
rad_bal_total_with_topography <- function (...) {
  UseMethod("rad_bal_total_with_topography")
}

#' @rdname rad_bal_total_with_topography
#' @method rad_bal_total_with_topography numeric
#' @param rad_sw_balance_topography Shortwave radiation balance in dependency of topography in W/m^2.
#' @param rad_lw_surface Longave surface emissions in W/m^2.
#' @param rad_lw_atmospheric Atmospheric radiation in W/m^2.
#' @param terr_sky_view Sky view factor from 0-1.
rad_bal_total_with_topography.numeric <- function(rad_sw_balance_topography,
                                          rad_lw_surface,
                                          rad_lw_atmospheric,
                                          terr_sky_view){
  # Longwave component:
  G_topo <- rad_lw_atmospheric*terr_sky_view + rad_lw_surface*(1-terr_sky_view)
  lw_bal_topo <- rad_lw_surface - G_topo

  rad_bal_topo <- rad_sw_balance_topography - lw_bal_topo
  return(rad_bal_topo)
}

