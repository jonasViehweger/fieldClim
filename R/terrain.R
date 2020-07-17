#' Sky view factor
#'
#' Calculates the sky view factor of a position only based on slope angle and
#' position in valley or on a slope.
#'
#' Terrain view factor can be calculated by 1-terr_sky_view().
#'
#' @param slope Inclination of slope in degrees.
#' @param valley If the position is in a valley (TRUE) or on a slope (FALSE).
#'
#' @return Sky view factor from 0-1.
#' @export
#'
#' @examples
terr_sky_view <- function(slope, valley = F) {
  f <- pi/180                                 # Winkel in Radiant Faktor
  if(valley) return((1+cos(slope*f))/2.0)
  return(cos(slope*f))
}
