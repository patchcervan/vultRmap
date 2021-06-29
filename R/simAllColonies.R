#' Simulate activity for all colonies
#'
#' @description This is a wrapper around simTrips that does all necessary
#' computation for simulating activity around all colonies.
#' @param col_to_pred A data frame with the colonies to predict from. These
#' should generally come from the auxiliary data coming with the package or at
#' least have a similar structure.
#' @param totalsteps Total number of steps we want to simulate
#' @param dist_lim Maximum distance vultures are allowed to travel away from
#' its central colony (col_sel).
#' @param age Age of the vultures: "ad" for adult, "juv" for juveniles
#' @param sample_coefs Whether step-selection coefficients should be sampled
#' from the distribution of random effects. Either NULL in which case the mean
#' of the distribution of effects will be used, or a number specifying the
#' number of coefficients to sample.
#' @param set_seed Specifies whether a random seed should be specified. Either
#' NULL for no random seed or a number specifying the seed to use.
#' @param ncores Specifies the number of cores that should be used for the
#' simulations.
#' @param data_dir A character string with the path to the directory containing
#' the auxiliary data needed for the simulations.
#' @param out_dir Specifies the directory where the simulation results should
#' be saved to.
#'
#' @return A data frame with at least the desired number of steps (the same
#' number of steps are allocated to cores and rounded up) for each colony. Due
#' to potentially large output an output directory must be specified where a data
#' frame for each colony will be saved.
#' @export
#'
#' @examples
simAllColonies <- function(col_to_pred, totalsteps, dist_lim, age,
                           sample_coefs, set_seed, ncores,
                           data_dir, out_dir){

  # Configure multicore
  if(ncores > 1){
    future::plan("multisession", workers = ncores)
  }

  # Define simulation parameters --------------------------------------------

  if(!is.null(set_seed)){
    set.seed(set_seed)
  }

  # Sample random coefficients if necessary
  if(is.null(sample_coefs)){
    ssf_coef <- vultRmap::sampleSsfCoef(nind = 1, seed = set_seed)
  } else {
    ssf_coef <- vultRmap::sampleSsfCoef(nind = sample_coefs, seed = set_seed)
  }

  # Number of steps per core
  nsteps <- rep(ceiling(totalsteps/ncores), ncores) # rounded up, so that we get AT LEAST the desired number of steps


  # Read in necessary data --------------------------------------------------

  # We will need to calculate distance to other colonies
  col_all <- utils::read.csv(paste0(data_dir, "/colony_data.csv"))

  # And to supplementary feeding sites
  sfs <- utils::read.csv(paste0(data_dir, "/sup_feeding_data.csv"))

  # Loop through colonies ---------------------------------------------------

  for(i in 1:nrow(col_to_pred)){

    col_sel <- col_to_pred[i,]

    # Prepare habitat for simulations

    # This may take some minutes if max_range is large (>500)
    hab <- vultRmap::prepColHab(col_cc = unlist(col_sel[, c("lon","lat")]), max_range = dist_lim,
                                col_all = col_all, sfs = sfs, scale = "ssf")

    hab <- vultRmap::completeDataFrame(hab, names(ssf_coef[[1]]), 0)


    # Simulate activity

    gc()

    # Simulate
    sims <- furrr::future_map2_dfr(nsteps, ssf_coef,
                                   ~vultRmap::simTrips(.nsteps = .x, .ssf_coef = .y,
                                                       .age = age, .hab = hab,
                                                       .mov_ker = vultRmap::mov_kernel,
                                                       .col_sel = unlist(col_sel[, c("lon","lat")]),
                                                       .maxdist = (dist_lim - dist_lim * 10/100)),
                                   .options = furrr::furrr_options(seed = set_seed))

    saveRDS(sims, paste0(out_dir, "/", col_sel$id, "_", age, "_sims.rds"))

    rm(sims, hab)

    gc()

  }

  if(ncores > 1){
    future::plan("sequential")
  }

}
