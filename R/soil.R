#' Soil thermal conductivity
#'
#' Calculates soil thermal conductivity (W/m K) from soil moisture (Vol-%) and texture.
#'
#' Works by linearly interpolating thermal conductivity based on measured data.
#'
#' @rdname soil_thermal_cond
#' @param ... Additional parameters passed to later functions.
#' @return Soil thermal conductivity in W/m K.
#' @export
#'
soil_thermal_cond <- function (...) {
  UseMethod("soil_thermal_cond")
}

#' @rdname soil_thermal_cond
#' @method soil_thermal_cond numeric
#' @param moisture Soil moisture in Vol-%.
#' @param texture Soil texture. Either "sand" or "clay".
#' @importFrom stats approx
#' @export
#'
soil_thermal_cond.numeric <- function(moisture, texture = "sand", ...) {
  if(texture == "sand"){
    y <- c(0.269,1.46,1.98,2.18,2.31,2.49,2.58)
  } else if(texture == "clay"){
    y <- c(0.276,0.586,1.1,1.43,1.57,1.74,1.95)
  } else {
    stop("Texture not available. Input either 'sand' or 'clay'")
  }
  x <- c(0, 5, 10, 15, 20, 30, 43)

  # linear interpolation of values
  therm_cond <- approx(x, y, xout = moisture, yleft = NA, yright = y[7])
  return(therm_cond$y)
}

#' @rdname soil_thermal_cond
#' @method soil_thermal_cond weather_station
#' @param weather_station Object of class weather_station.
#' @export
#'
soil_thermal_cond.weather_station <- function(weather_station, ...) {
  check_availability(weather_station, "moisture", "texture")
  moisture <- weather_station$measurements$moisture
  texture <- weather_station$location_properties$texture
  return(soil_thermal_cond(moisture, texture))
}


#' Soil volumetric heat capacity
#'
#' Calculates soil volumetric heat capacity (J / (m^3 * K)) from soil moisture (Vol-%) and texture.
#'
#' Works by linearly interpolating volumetric heat capacity based on measured data.
#'
#' @rdname soil_heat_cap
#' @param ... Additional parameters passed to later functions.
#' @return Numeric vector with volumetric heat capacity in J/(m^3 * K)
#' @export
#'
soil_heat_cap <- function (...) {
  UseMethod("soil_heat_cap")
}


#' @rdname soil_heat_cap
#' @method soil_heat_cap numeric
#' @param moisture Soil moisture in Vol-%.
#' @param texture Soil texture. Either "sand" or "clay".
#' @importFrom stats approx
#' @export
soil_heat_cap.numeric <- function(moisture, texture = "sand", ...) {
  if(texture == "sand"){
    y <- c(1.17,1.38,1.59,1.8,2.0,2.42,2.97)
  } else if(texture == "clay"){
    y <- c(1.19,1.4,1.61,1.82,2.03,2.45,2.99)
  } else {
    stop("Texture not available. Input either 'sand' or 'clay'")
  }
  x <- c(0, 5, 10, 15, 20, 30, 43)

  # linear interpolation of values
  vol_heat <- approx(x, y, xout = moisture, yleft = NA, yright = y[7])
  return(vol_heat$y)
}



#' @rdname soil_heat_cap
#' @method soil_heat_cap weather_station
#' @param weather_station Object of class weather_station.
#' @export
#'
soil_heat_cap.weather_station <- function(weather_station, ...) {
  check_availability(weather_station, "moisture", "texture")
  moisture <- weather_station$measurements$moisture
  texture <- weather_station$location_properties$texture
  return(soil_heat_cap(moisture, texture))
}


#' Soil heat flux
#'
#' Calculates soil heat flux from measurements in two different
#' depths and thermal conductivity of the soil.
#'
#' Negative values signify flux towards the atmosphere, positive values signify flux into the soil.
#'
#' @rdname soil_heat_flux
#' @param ... Additional parameters passed to later functions.
#' @return Soil heat flux in W*m^-2.
#' @export
#'
soil_heat_flux <- function (...) {
  UseMethod("soil_heat_flux")
}

#' @rdname soil_heat_flux
#' @method soil_heat_flux numeric
#' @export
#' @param ts1 Upper soil temperature (closest to the surface) in degrees C.
#' @param ts2 Lower soil temperature in degrees C.
#' @param depth1 Depth of upper measurement (closest to the surface) in m.
#' @param depth2 Depth of lower measurement in m.
#' @param thermal_cond Thermal conductivity of soil in W/m K.
soil_heat_flux.numeric <- function(ts1, ts2, depth1, depth2, thermal_cond, ...) {
  return (thermal_cond*((ts1-ts2)/(depth2-depth1)))
}

#' @rdname soil_heat_flux
#' @method soil_heat_flux weather_station
#' @export
#' @param weather_station Object of class weather_station.
soil_heat_flux.weather_station <- function(weather_station, ...) {
  check_availability(weather_station, "ts1", "ts2", "depth1", "depth2")
  ts1 <- weather_station$measurements$ts1
  ts2 <- weather_station$measurements$ts2
  depth1 <- weather_station$properties$depth1
  depth2 <- weather_station$properties$depth2
  thermal_cond <- soil_thermal_cond(weather_station)
  return (soil_heat_flux(ts1, ts2, depth1, depth2, thermal_cond))
}


#' Soil attenuation length
#'
#' Calculates soil attenuation length.
#'
#' @rdname soil_attenuation
#' @param ... Additional parameters passed to later functions.
#' @return Soil attenuation length in m.
#' @export
#'
soil_attenuation <- function (...) {
  UseMethod("soil_attenuation")
}

#' @rdname soil_attenuation
#' @method soil_attenuation numeric
#' @export
#' @param thermal_cond Thermal conductivity of soil in W/m K.
#' @param vol_heat_cap Volumetric heat capacity of soil in J/(m^3 * K).
soil_attenuation.numeric <- function(thermal_cond, vol_heat_cap, ...) {
  return(sqrt((thermal_cond*24)/(vol_heat_cap*pi)))
}

#' @rdname soil_attenuation
#' @method soil_attenuation weather_station
#' @export
#' @param weather_station Object of class weather_station.
soil_attenuation.weather_station <- function(weather_station, ...) {
  thermal_cond <- soil_thermal_cond(weather_station)
  vol_heat_cap <- soil_heat_cap(weather_station)
  return(soil_attenuation(thermal_cond, vol_heat_cap))
}
