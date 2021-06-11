#' Sample step-selection coefficients
#'
#' @description Sample random step-selection coefficients from a hierarchical step-selection model
#' @param mod_summary The summary of a glmmTMB model. If not specified, the ssf_fit_summary, contained in the package data will be used
#' @param nind Number of individuals to sample for
#' @param seed Whether a seed should be specified to the random selection of coefficients
#'
#' @return A list with each element being a vector of step-selection coefficients
#' @export
#'
#' @examples
#' vultRmap::sampleSsfCoef(nind = 2)
sampleSsfCoef <- function(mod_summary = ssf_fit_summary, nind = 1, seed = NULL){

  # Extract model coefficients
  ssf_coef <- mod_summary$coefficients$cond[,"Estimate"]
  mov_coef <- ssf_coef[grepl("sl_", names(ssf_coef))]

  # Sample from random effects
  sd_eff <- mod_summary$varcor$cond
  sd_eff <- sqrt(do.call("c", sd_eff))

  names(sd_eff) <- sapply(mod_summary$varcor$cond, function(x) attr(x, "dimnames")[[1]])
  sd_eff <- sd_eff[names(sd_eff)!= "(Intercept)"]

  ssf_coefs <- vector("list", nind)
  ssf_coefs[[1]] <- ssf_coef

  # Sample coefficients

  if(!is.null(seed)){
    set.seed(seed)
  }

  if(nind > 1){
    for(j in 2:nind){
      for(i in 1:length(ssf_coef)){
        idx <- which(names(sd_eff) == names(ssf_coef)[i])
        if(length(idx) != 0){
          ssf_coefs[[j]][i] <- ssf_coef[i] + stats::rnorm(1, 0, sd = sd_eff[idx])
        } else {
          ssf_coefs[[j]][i] <- ssf_coef[i]
        }
      }

      attributes(ssf_coefs[[j]]) <- attributes(ssf_coef)
    }
  }

  return(ssf_coefs)

}
