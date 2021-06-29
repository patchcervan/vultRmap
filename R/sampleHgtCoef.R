#' Sample height flight model coefficients
#'
#' @description Sample random height flight model coefficients from a
#' hierarchical height flight model.
#' @param mod_summary The summary of a glmmTMB model. If not specified,
#' the hgt_fit_summary, contained in the package data will be used
#' @param nind Number of individuals to sample for
#' @param seed Whether a seed should be specified to the random selection of
#' coefficients
#'
#' @return A list with each element being a vector of step-selectioncoefficients
#' @export
#'
#' @examples
#' vultRmap::sampleHgtCoef(nind = 2)
sampleHgtCoef <- function(mod_summary = hgt_fit_summary, nind = 1, seed = NULL){

  # Extract model coefficients
  hgt_coef <- mod_summary$coefficients$cond[,"Estimate"]

  # Sample from random effects
  sd_eff <- mod_summary$varcor$cond
  sd_eff <- sqrt(do.call("c", sd_eff))

  names(sd_eff) <- sapply(mod_summary$varcor$cond, function(x) attr(x, "dimnames")[[1]])

  hgt_coefs <- vector("list", nind)
  hgt_coefs[[1]] <- hgt_coef

  # Sample coefficients

  if(!is.null(seed)){
    set.seed(seed)
  }

  if(nind > 1){
    for(j in 2:nind){
      for(i in 1:length(hgt_coef)){
        idx <- which(names(sd_eff) == names(hgt_coef)[i])
        if(length(idx) != 0){
          hgt_coefs[[j]][i] <- hgt_coef[i] + stats::rnorm(1, 0, sd = sd_eff[idx])
        } else {
          hgt_coefs[[j]][i] <- hgt_coef[i]
        }
      }

      attributes(hgt_coefs[[j]]) <- attributes(hgt_coef)
    }
  }

  return(hgt_coefs)

}
