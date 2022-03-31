#' Simulate activity from step-selection
#'
#' @param colonies Either a vector of colony codes to simulate activity around
#' or "all" to simulate activity around all colonies that meet certain criteria.
#' At the moment these are breeding colonies for which there are counts and roots
#' for which there are counts of more than 50 birds.
#' @param out_dir Directory where the output of the simulations should go to
#' @param data_dir Directory where the colony data is.
#' @param seeds A vector of two seeds that will be used in the simulations
#' @param ... Other parameters to be passed on to [vultRmap](simAllColonies)
#'
#' @return
#' @export
#'
#' @examples
sim_ssf <- function(colonies = "all", out_dir, data_dir, seeds, ...){

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


  # In case we want to exclude some colonies
  # col_to_pred <- col_to_pred[-(1:3),]

  # Define ages
  ages <- c("ad", "juv")

  # And output suffixes (there are two rounds of simulations)
  suff <- c("", "_v2")

  for(k in 1:2){
    for(j in 1:2){

      vultRmap::simAllColonies(col_to_pred = col_to_pred,
                               age = ages[j],
                               set_seed = seeds[j],
                               out_dir = out_dir,
                               data_dir = data_dir,
                               out_suffix = suff[k],
                               ...)

    }
  }



}
