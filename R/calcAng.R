#' Calculate POSITIVE angles
#'
#' @description Calculates an angle between `[0, 2*pi]` from the x-axis to the
#' vector that goes from the origin to the point x, y
#' @param y A vector y-coordinates
#' @param x A vector of x-coordinates
#' @param degrees If TRUE angles are given in degrees, otherwise (default) they
#' are given in radians
#'
#' @return A vector equal length to x and y of angles. x and y coordinates are
#' used by pairs (i.e. the first vector has coordinates `c(x[1], y[1])`, the
#' second `c(x[2], y[2])`, etc)/
#' @export
#'
#' @examples
#' x <- seq(-3, 3, 1); y = seq(-3, 3, 1)
#' calcAng(y, x)
calcAng <- function(y, x, degrees = FALSE){

  comb <- 1

  if(degrees){
    comb <- 360/(2*pi)
  }

  a <- atan2(y, x)

  ang <- (a < 0)*(2*pi) + a

  return(ang * comb)

}
