#' Calculate utilization distribution quantile
#'
#' @description Calculates the CDF of a utilization distribution, passed on as
#' a vector of densities/probabilities. It returns the highest value that
#' captures the desired level of activity proportion.
#' @param ud A vector of utilization intensity values
#' @param probs A vector of proportions/probabilities.
#'
#' @return A vector with the same length of probs, representing the utilization
#' intensity values that capture the desired proportion of activity/probability
#' of occurrence.
#' @export
#'
#' @examples
calcUDquantile <- function(ud, probs){

      # Calculate the distribution function
      ud <- sort(ud)
      cumud <- cumsum(ud)/sum(ud)

      # Calculate the minimum value that captures at least a UD  proportion of probs
      qs <- sapply(1-probs, function(x) min(ud[cumud >= x]))

      return(qs)

}
