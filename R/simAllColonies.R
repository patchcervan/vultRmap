#' Simulate activity for all colonies
#'
#' @description This is a wrapper around simOneColony that does all necessary
#' computation for simulating activity around all colonies.
#' @param age Age of the vultures: "ad" for adult, "juv" for juveniles.
#' @param min_size_col Simulations will only be produced for colonies with number of
#' adults greater or equal to min_size_col.
#' @param min_size_roost Simulations will only be produced for roosts with number of
#' birds (adults + juveniles) greater or equal to min_size_roost.
#' @param out_dir Specifies the directory where the simulation results should
#' be saved to.
#' @param data_dir A character string with the path to the directory containing
#' the auxiliary data needed for the simulations.
#' @param ... Other arguments passed on to simOneColony.
#'
#' @return A data frame with at least the desired number of steps (the same
#' number of steps are allocated to cores and rounded up) for each colony. Due
#' to potentially large output an output directory must be specified where a data
#' frame for each colony will be saved.
#' @export
#'
#' @examples
simAllColonies <- function(age, min_size_col = 1, min_size_roost = 50,
                           out_dir, data_dir, ...){

  # Read in necessary data --------------------------------------------------

  # We will need to calculate distance to other colonies
  col_all <- utils::read.csv(paste0(data_dir, "/colony_data.csv"))

  # Subset colonies to we have counts for
  col_to_pred <- col_all %>%
    dplyr::filter(!is.na(avg_ad)) %>%
    dplyr::filter((type == "breed" & avg_ad >= min_size_col) |
                    (type == "roost" & (avg_ad + avg_juv) >= min_size_roost))

  # this is just for testing (remove)
  # col_to_pred <- col_to_pred[1:3, ]

  # Loop through colonies

  for(i in 1:nrow(col_to_pred)){

    col_sel <- col_to_pred[i,]

    gc()

    # Simulate
    sims <- vultRmap::simOneColony(col_sel = unlist(col_sel[ , c("lon", "lat")]),
                                   data_dir = data_dir, age = age, ...)

    saveRDS(sims, paste0(out_dir, "/", col_sel$id, "_", age, "_sims.rds"))

    rm(sims)

    gc()

  }

}
