#' Make binned raster map
#'
#' @description Take a raster with continuous values and create a discrete version
#' by binning those values according to quantiles of their cumulative distribution
#' @param x a Raster object.
#' @param nlevels numeric value representing the number of bins the values should
#' be divided into. If specified `breaks` will be ignored.
#' @param breaks a numeric vector with the quantiles defining each of the bins the
#' values of `x` should be divided into.
#' @param cutoff numeric, quantile of the cumulative distribution above which any value
#' will be classified as NA and will not appear in the binned version of the raster.
#' Defaults to 1.
#'
#' @return A Raster object that is a binned version of `x`.
#' @export
#'
#' @examples
makeBinnedRasterMap <- function(x, nlevels = NULL, breaks = NULL, cutoff = 1){

  # Remove 0.1% of activity to reduce mapping
  low_quant <- calcUDquantile(raster::getValues(x), cutoff)

  # Here is the thing. Between these two commands
  # x[x < low_quant] <- NA
  x <- raster::clamp(x, lower = low_quant, useValues = FALSE)

  if(!is.null(breaks)){
    if(!is.null(nlevels)){
      stop("Either `breaks` OR `nlevels` must be specified")
    }

    udlevels <- breaks

    if(max(breaks) < 1){
      udlevels <- c(udlevels, 1)
      pad_high <- TRUE
    }

    if(min(breaks) > 0){
      udlevels <- c(0, udlevels)
      pad_low <- TRUE
    }

    udlevels <- sort(udlevels)

  } else if(is.null(nlevels)){
    stop("Either `breaks` OR `nlevels` must be specified")
  } else{
    udlevels <- seq(0, 1, length.out = nlevels + 1)
  }

  bins <- calcUDquantile(raster::getValues(x), udlevels)
  bins[which.min(bins)] <- 0

  rcl_vals <- rep(rev(bins), each = 2)
  rcl_vals <- rcl_vals[c(-1, -length(rcl_vals))]
  rcl_mat <- matrix(rcl_vals, ncol = 2, byrow = TRUE)
  rcl_mat <- cbind(rcl_mat, rev(udlevels[-1]))

  if(exists("pad_high") && pad_high){
    rcl_mat[1, 3] <- NA
  }

  if(exists("pad_low") && pad_low){
    rcl_mat[length(udlevels) - 1, 3] <- NA
  }

  dens_discrete <- raster::reclassify(x, rcl_mat, include.lowest = TRUE)
  # dens_discrete <- raster::as.factor(dens_discrete)

  return(dens_discrete)

}
