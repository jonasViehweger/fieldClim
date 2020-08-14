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
