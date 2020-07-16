#' Emissivity of the atmosphere
#'
#' @param temp numeric. Air termperature in degrees celsius.
#' @param altitude numeric. Meters above sea level
#' @param air_pressure numeric. Optional. Air pressure in hPa.
#' If not available, will be calculated from altitude and air temperature.
#'
#' @return numeric. Emissivity of the atmosphere (0-1)
#' @export
#'
#' @examples
rad_emissivity_air <- function(temp, altitude, air_pressure = NULL){
  if(is.null(air_pressure)) p <- pres_p(altitude, temp)
  svp <- hum_sat_vapor_pres(temp)
  t_over <- temp*(0.0065*altitude)
  eat <- ((1.24*svp/temp)**1/7)*(p/1013.25)
  return(eat)
}

#' Longwave radiation of the surface
#'
#' Calculates emissions of a surface.
#'
#' @param temp numeric. Surface temperature in degrees celsius
#' @param emissivity numeric. Emissivity of surface. Default is emissivity for short grass
#'
#' @return Emissions in W*m^-2
#' @export
#'
#' @examples
rad_lw_surface <- function(temp, emissivity = 0.95){
  sigma <- 5.670374 * 10^-8
  return(em*sigma*(temp+273.15)**4)
}

#' Longwave radiation of the atmosphere
#'
#' @param emissivity_air numeric. Emissivity of the atmosphere (factor: 0-1)
#' @param temp numeric. Air temperature in degrees celsius
#'
#' @return numeric. Atmospheric radiation in W*m^-2
#' @export
#'
#' @examples
rad_lw_atmospheric <- function(emissivity_air, temp){
  sigma <- 5.670374 * 10^-8
  gs <- emissivity_air*sigma*(temp+273.15)**4
  return(gs)
}


#' Shortwave radiation at top of atmosphere.
#'
#' Calculation of the shortwave radiation at the top of the atmosphere.
#'
#' @param datetime Date and time.
#' @param lat Latitude of the place of the climate station in decimal degrees.
#' @param lon Longitude of the place of the climate station in decimal degrees.
#'
#' @return Shortwave radiation at top of atmosphere in W/m^2.
#' @export
#'
#' @examples
rad_sw_toa <- function(datetime, lat, lon){
  sol_const <- 1367
  sol_eccentricity <- sol_eccentricity(datetime)
  sol_elevation <- sol_elevation(datetime,lat,lon)
  rad_sw_toa <- sol_const*sol_eccentricity*sin(sol_elevation*(pi/180))
  return(rad_sw_toa)
}

#' Shortwave radiation onto a horizontal area on the ground.
#'
#' Calculation of the shortwave radiation onto a horizontal area on the ground.
#'
#' @param rad_sw_toa Shortwave radiation at top of atmosphere in W/m^2.
#' @param trans_total Total transmittance (0-1)
#'
#' @return Shortwave radiation on the ground onto a horizontal area in W/m^2.
#' @export
#'
#' @examples
rad_sw_ground_horizontal <- function(rad_sw_toa, trans_total){
  rad_sw_ground_horizontal <- rad_sw_toa*trans_total
  return(rad_sw_ground_horizontal)
}

#' Reflected shortwave radiance (albedo radiance).
#'
#' Calculation of the reflected shortwave radiance (albedo radiance).
#'
#' @param rad_sw_ground_horizontal Shortwave radiation on the ground onto a horizontal area in W/m^2.
#' @param albedo Albedo of the surface.
#'
#' @return Reflected shortwave radiance in W/m^2.
#' @export
#'
#' @examples
rad_sw_reflected <- function(rad_sw_ground_horizontal, albedo){
  rad_sw_reflected <- rad_sw_ground_horizontal*albedo
  return(rad_sw_reflected)
}

#' Shortwave radiation balance.
#'
#' Calculation of the shortwave radiation balance.
#'
#' @param rad_sw_ground_horizontal Shortwave radiation on the ground onto a horizontal area in W/m^2.
#' @param rad_sw_reflected Reflected shortwave radiance in W/m^2.
#'
#' @return Shortwave radiation balance in W/m^2.
#' @export
#'
#' @examples
rad_sw_radiation_balance <- function(rad_sw_ground_horizontal, rad_sw_reflected){
  rad_sw_radiation_balance <- rad_sw_ground_horizontal-rad_sw_reflected
  return(rad_sw_radiation_balance)
}

#' Shortwave radiation balance in dependency of topography.
#'
#' Calculate shortwave radiation balance in dependency of topography.
#'
#' @param slope Slope in degrees.
#' @param valley Is the climate station placed in a valley (True/False)?
#' @param terr_sky_view Sky view factor from 0-1.
#' @param sol_elevation Sun elevation.
#' @param sol_azimuth Sun atzimuth.
#' @param exposition Exposition (North = 0, South = 180)
#' @param rad_sw_ground_horizontal Shortwave radiation on the ground onto a horizontal area in W/m^2.
#' @param albedo Albedo of surface.
#'
#' @return Shortwave radiation balance in dependency of topography in W/m^2.
#' @export
#'
#' @examples
rad_sw_reflected_by_terrain <- function(slope, valley = F, terr_sky_view, sol_elevation, sol_azimuth, exposition, rad_sw_ground_horizontal, albedo){
  sol_dir <- rad_sw_ground_horizontal/0.9751
  sol_dif <- rad_sw_ground_horizontal-sol_dif
  if(slope > 0) {
    terrain_angle <- cos(slope*(pi/180))*sin(sol_elevation*(pi/180))+sin(slope*(pi/180))*cos(sol_elevation*(pi/180))*cos(sol_azimuth*(pi/180)-(exposition*(pi/180)))/(pi/180)
    print("Topographische Direktstrahlung in W/m?")
    rad_sw_topo_direct <- (sol_dir/sin(sol_elevation*(pi/180)))*terrain_angle
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
  sol_ter <- (rad_sw_topo_direct + rad_sw_topo_diffuse) * albedo * terr_sky_view(slope, valley)
  return(sol_ter)
  }
