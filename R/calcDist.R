#' Calculate Euclidean distance
#' @description Helper function to vectorize distance calculations over multiple points
#' @param s A numeric vector with two coordinates c(x, y), in this order, that will be used as the point to calculate distance from.
#' @param x A numeric  vector of x-coordinates of the points to calculate distance to. Same length as y.
#' @param y A numeric vector of y-coordinates of the points to calculate distance to. Same length as x.
#'
#' @return A vector of distances as long as the length of x and y.
#' @export
#'
#' @examples
#' calcDist(c(0, 0), c(1, 2, 3), c(1, 2, 3))
calcDist <- function(s, x, y){
  sqrt((s[1] - x)^2 + (s[2] - y)^2)
}
