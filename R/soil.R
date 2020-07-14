#' Soil thermal conductivity
#'
#' Calculates soil thermal conductivity (W/m K) from soil moisture (Vol-%) and texture.
#'
#' Works by linearly interpolating thermal conductivity based on measured data.
#'
#' @param moisture Numeric vector with soil moisture data (in Vol-%)
#' @param texture Soil texture. Either "sand" or "clay".
#'
#' @return vector with soil thermal conductivity (W/m K)
#' @export
#'
#' @examples
soil_thermal_cond <- function(moisture, texture = "sand") {
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


#' Soil volumetric heat capacity
#'
#' Calculates soil volumetric heat capacity \eqn{J * m^(-3) * K^(-1)} from soil moisture (Vol-%) and texture.
#'
#' Works by linearly interpolating volumetric heat capacity based on measured data.
#'
#' @param moisture Numeric vector with soil moisture data (in Vol-%)
#' @param texture Soil texture. Either "sand" or "clay".
#'
#' @return Numeric vector with volumetric heat capacity \eq{J*m^-3*K^-1}
#' @export
#'
#' @examples
soil_heat_cap <- function(moisture, texture = "sand") {
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


#' Soil heat flux
#'
#' Calculates soil heat flux from measurements in two different
#' depths and thermal conductivity of the soil.
#'
#' Negative values signify flux towards the atmosphere, positive values signify flux into the soil.
#'
#' @param diff_temp Difference in temperature between two depths.
#' @param diff_depth Difference in depths (m)
#' @param thermal_cond Thermal conductivity of soil (W/m K)
#'
#' @return Soil heat flux (W*m^-2)
#' @export
#'
#' @examples
soil_heat_flux <- function(diff_temp, diff_depth, thermal_cond) {
  if (!is.numeric(diff_temp)) stop("diff_temp is not numeric")
  if (!is.numeric(diff_depth)) stop("diff_depth is not numeric")
  if (!is.numeric(thermal_cond)) stop("thermal_cond is not numeric")
  if (any(thermal_cond < 0)){
    warning("Negative thermal_cond values will be converted to NA.")
    thermal_cond[thermal_cond < 0] <- NA
  }
  return (thermal_cond*(diff_temp/diff_depth))
}


#' Soil attenuation length
#'
#' @param thermal_cond Thermal conductivity of soil (W/m K)
#' @param vol_heat_cap Volumetric heat capacity of soil \eq{J*m^-3*K^-1}
#'
#' @return Soil attenuation length (m)
#' @export
#'
#' @examples
soil_attenuation <- function(thermal_cond, vol_heat_cap) {
  if (!is.numeric(vol_heat_cap)) stop("vol_heat_cap is not numeric")
  if (!is.numeric(thermal_cond)) stop("thermal_cond is not numeric")
  if (any(thermal_cond < 0)){
    warning("Negative thermal_cond values will be converted to NA.")
    thermal_cond[thermal_cond < 0] <- NA
  }
  if (any(vol_heat_cap < 0)){
    warning("Negative vol_heat_cap values will be converted to NA.")
    vol_heat_cap[vol_heat_cap < 0] <- NA
  }
  return(sqrt((thermal_cond*24)/(vol_heat_cap*pi)))
}
