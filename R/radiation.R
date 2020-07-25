#' Emissivity of the atmosphere
#'
#' Calculation of the emissivity of the atmosphere.
#'
#' @param t Air temperature in degrees C.
#' @param elev Meters above sea level
#' @param p OPTIONAL. Air pressure in hPa.
#' If not available, will be calculated from elev and air temperature.
#'
#' @return Emissivity of the atmosphere (0-1).
#' @export
#'
rad_emissivity_air <- function(t, elev, p = NULL){
  if(is.null(p)) p <- pres_p(elev, t)
  svp <- hum_sat_vapor_pres(t)
  t_over <- t*(0.0065*elev)
  eat <- ((1.24*svp/t)**1/7)*(p/1013.25)
  return(eat)
}

#' Longwave radiation of the surface
#'
#' Calculates emissions of a surface.
#'
#' @param t Surface temperature in degrees C.
#' @param emissivity_surface Emissivity of surface.
#' Default is emissivity for short grass.
#'
#' @return Emissions in W/m^2.
#' @export
#'
rad_lw_surface <- function(t, emissivity_surface = 0.95){
  sigma <- 5.670374e-8
  return(emissivity_surface*sigma*(t+273.15)**4)
}

#' Longwave radiation of the atmosphere
#'
#' Calculation of the longwave radiation of the atmosphere.
#'
#' @param emissivity_air Emissivity of the atmosphere (factor: 0-1)
#' @param t Air temperature in degrees C.
#'
#' @return Atmospheric radiation in W/m^2.
#' @export
#'
rad_lw_atmospheric <- function(emissivity_air, t){
  sigma <- 5.670374e-8
  gs <- emissivity_air*sigma*(t+273.15)**4
  return(gs)
}


#' Shortwave radiation at top of atmosphere
#'
#' Calculation of the shortwave radiation at the top of the atmosphere.
#'
#' @param datetime Date and time as POSIX type.
#' See [base::as.POSIXlt()] and [base::strptime] for conversion.
#' @param lat Latitude of the place of the climate station in decimal degrees.
#' @param lon Longitude of the place of the climate station in decimal degrees.
#'
#' @return Shortwave radiation at top of atmosphere in W/m^2.
#' @export
#'
rad_sw_toa <- function(datetime, lat, lon){
  if(!inherits(datetime, "POSIXt")){
    stop("datetime has to be of class POSIXt.")
  }
  sol_const <- 1367
  sol_eccentricity <- sol_eccentricity(datetime)
  sol_elevation <- sol_elevation(datetime, lat, lon)
  rad_sw_toa <- sol_const*sol_eccentricity*sin(sol_elevation*(pi/180))
  return(rad_sw_toa)
}

#' Shortwave radiation onto a horizontal area on the ground
#'
#' Calculation of the shortwave radiation onto a horizontal area on the ground.
#'
#' @param rad_sw_toa Shortwave radiation at top of atmosphere in W/m^2.
#' @param trans_total Total transmittance of the atmosphere (0-1).
#'
#' @return Shortwave radiation on the ground onto a horizontal area in W/m^2.
#' @export
#'
rad_sw_ground_horizontal <- function(rad_sw_toa, trans_total){
  rad_sw_ground_horizontal <- rad_sw_toa*trans_total
  return(rad_sw_ground_horizontal)
}

#' Reflected shortwave ratdiation
#'
#' Calculation of the reflected shortwave radiation.
#'
#' @param rad_sw_ground_horizontal Shortwave radiation on the ground onto a horizontal area in W/m^2.
#' @param albedo Albedo of the surface.
#'
#' @return Reflected shortwave ratdiation in W/m^2.
#' @export
#'
rad_sw_reflected <- function(rad_sw_ground_horizontal, albedo){
  rad_sw_reflected <- rad_sw_ground_horizontal*albedo
  return(rad_sw_reflected)
}

#' Shortwave radiation balance
#'
#' Calculation of the shortwave radiation balance.
#'
#' @param rad_sw_ground_horizontal Shortwave radiation on the ground onto a horizontal area in W/m^2.
#' @param rad_sw_reflected Reflected shortwave ratdiation in W/m^2.
#'
#' @return Shortwave radiation balance in W/m^2.
#' @export
#'
rad_sw_radiation_balance <- function(rad_sw_ground_horizontal, rad_sw_reflected){
  rad_sw_radiation_balance <- rad_sw_ground_horizontal-rad_sw_reflected
  return(rad_sw_radiation_balance)
}

#' Shortwave radiation balance in dependency of topography
#'
#' Calculate shortwave radiation balance in dependency of topography.
#'
#' @param slope Slope in degrees.
#' @param valley Is the climate station placed in a valley (TRUE/FALSE)?
#' @param sol_elevation Sun elevation in degrees.
#' @param sol_azimuth Sun azimuth in degrees.
#' @param exposition Exposition (North = 0, South = 180)
#' @param rad_sw_ground_horizontal Shortwave radiation on the ground onto a horizontal area in W/m^2.
#' @param albedo Albedo of surface.
#'
#' @return Shortwave radiation balance in dependency of topography in W/m^2.
#' @export
#'
rad_sw_balance_topography <- function(slope, valley = F,
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
#' @param rad_sw_radiation_balance Shortwave radiation balance in W/m^2.
#' @param rad_lw_surface Longave surface emissions in W/m^2.
#' @param rad_lw_atmospheric Atmospheric radiation in W/m^2.
#'
#' @return Total radiation balance in W/m^2.
#' @export
#'
rad_bal_total <-function(rad_sw_radiation_balance, rad_lw_surface, rad_lw_atmospheric){
  radbil <- rad_sw_radiation_balance - (rad_lw_surface-rad_lw_atmospheric)
  return(radbil)
}


#' Total radiation balance with topography
#'
#' Calculates the total radiation balance with topography.
#'
#' @param rad_sw_balance_topography Shortwave radiation balance in dependency of topography in W/m^2.
#' @param rad_lw_surface Longave surface emissions in W/m^2.
#' @param rad_lw_atmospheric Atmospheric radiation in W/m^2.
#' @param terr_sky_view Sky view factor from 0-1.
#'
#' @return Total radiation balance with topography in W/m^2.
#' @export
#'
rad_bal_total_with_topography <- function(rad_sw_balance_topography,
                                          rad_lw_surface,
                                          rad_lw_atmospheric,
                                          terr_sky_view){
  # Longwave component:
  G_topo <- rad_lw_atmospheric*terr_sky_view + rad_lw_surface*(1-terr_sky_view)
  lw_bal_topo <- rad_lw_surface - G_topo

  rad_bal_topo <- rad_sw_balance_topography - lw_bal_topo
  return(rad_bal_topo)
}

