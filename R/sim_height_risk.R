#' Simulate risk height from step-selection activity simulations
#'
#' @param colonies Either a vector of colony codes to simulate activity around
#' or "all" to simulate activity around all colonies that meet certain criteria.
#' At the moment these are breeding colonies for which there are counts and roots
#' for which there are counts of more than 50 birds.
#' @param data_dir Directory where the colony data is.
#' @param sims_dir Directory where the step-selection activity simulations are stored
#' @param out_dir Directory where the output of the simulations should go to
#' @param ncoefs Number of random coefficients to be sampled from the risk height
#' model.
#' @param seed A random seed that will be used to sample random model coefficients
#'
#' @return
#' @export
#'
#' @examples
sim_height_risk <- function(colonies = "all", data_dir, sims_dir, out_dir, ncoefs, seed){

  # Define minimum size of colony (number of adults) and
  # roosts (total number of birds)
  min_size_col <- 1
  min_size_roost <- 50

  # We will need to calculate distance to other colonies
  col_all <- utils::read.csv(file.path(data_dir, "colony_data.csv"))
  # For debugging
  # col_all <- utils::read.csv("../vultRmap_data_aux/colony_data.csv")

  # Subset colonies to simulate around of
  if(colonies == "all"){

    # Subset colonies to we have counts for
    col_to_pred <- col_all %>%
      dplyr::filter(!is.na(avg_ad)) %>%
      dplyr::filter((type == "breed" & avg_ad >= min_size_col) |
                      (type == "roost" & (avg_ad + avg_juv) >= min_size_roost))

  } else {

    # Subset selected colonies
    col_to_pred <- col_all %>%
      dplyr::filter(id %in% colonies)

  }

  # Define ages
  ages <- c("ad", "juv")

  # Extract model coefficients (40 for each simulation file. Typically two)
  hgt_coef <- sampleHgtCoef(hgt_fit_summary, ncoefs*2, seed = seed)


  # Predict for all colonies ------------------------------------------------

  f <- function(x, j, .data_dir, .sims_dir, .out_dir){
    estHgtRisk(.col_sel = x,
               .coefs = hgt_coef, .age = ages[j],
               .datadir = .data_dir,
               .simsdir = .sims_dir,
               .outdir = .out_dir)
    gc()
  }

  for(j in 1:2){

    furrr::future_map(seq_len(nrow(col_to_pred)),
                      ~f(col_to_pred[.x, ], j, data_dir, sims_dir, out_dir),
                      .options = furrr::furrr_options(seed = TRUE))

  }
}
