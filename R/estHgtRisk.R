#' Estimate height risk
#'
#' @param .col_sel A named vector with the coordinates of the colony we want to
#' simulate around c("lon", "lat").
#' @param .coefs A list with samples of random coefficients. Each element in the
#' list corresponds to a set of coefficients for a different individual.
#' @param .datadir A character string with the path to the directory containing
#' the auxiliary data needed for the estimation (i.e. colony and supplementary
#' feeding sites data).
#' @param .simsdir A character string with the path to the directory containing
#' the points for which height risk wants to be estimated.
#' @param .outdir A character string with the path to the directory where the
#' results are to be stored at in disk. If NULL the results are not stored.
#'
#' @return
#' @export
#'
#' @examples
estHgtRisk <- function(.col_sel, .coefs, .datadir, .simsdir, .outdir){

      # Load sims and data
      .sims <- readRDS(paste0(simsdir, .col_sel$id, "_", age, "_sims.rds"))

      # We will need to calculate distance to other colonies
      .col_all <- utils::read.csv(paste0(.datadir, "/colony_data.csv"))

      # And to supplementary feeding sites
      .sfs <- utils::read.csv(paste0(.datadir, "/sup_feeding_data.csv"))

      # Identify individuals
      ind_id <- vector("integer", length = nrow(.sims))
      trip <- .sims$trip
      ind_id[1] <- 1

      for(i in seq_len(nrow(.sims) - 1)){
            if(trip[i+1] == 0 && trip[i] != 0){
                  ind_id[i+1] <- ind_id[i] + 1
            } else {
                  ind_id[i+1] <- ind_id[i]
            }
      }

      rm(trip)

      # Save coordinates
      cc <- .sims[, c("lon", "lat")]

      # Prepare colony habitat
      hab_col <- prepColHab(unlist(.col_sel[,c("lon", "lat")]),
                            max_range = max(.sims$dist_col)/1000 + 100,
                            col_all = .col_all,
                            sfs = .sfs, scale = "hgt")

      # Fix locations at colony, as it may not correspond to any cell centroid.
      # Move to closest location
      col_grid <- .sims %>%
            dplyr::filter(dist_col > 0) %>%
            dplyr::filter(dist_col == min(dist_col)) %>%
            dplyr::summarize(lon = unique(lon),
                             lat = unique(lat)) %>%
            dplyr::mutate(dist = calcDist(unlist(.col_sel[,c("lon", "lat")]), lon, lat)) %>%
            dplyr::filter(dist == min(dist)) %>%
            dplyr::distinct() %>%
            dplyr::select(lon, lat) %>%
            unlist()


      .sims <- .sims %>%
            dplyr::mutate(lon = ifelse(dist_col == 0, col_grid[1], lon),
                          lat = ifelse(dist_col == 0, col_grid[2], lat))

      # Keep only habitat within sims
      .sims <- .sims %>%
            dplyr::select(-dist_col) %>%
            mutate(lon = round(lon, 3),
                   lat = round(lat, 3)) %>%
            left_join(hab_col %>%
                            mutate(lon = round(lon, 3),
                                   lat = round(lat, 3)),
                      by = c("lon", "lat"))

      # Create data frame for prediction
      .sims <- .sims %>%
            dplyr::mutate('(Intercept)' = 1,
                   ttnoon_sq = ttnoon^2,
                   phi = 0) %>%
            dplyr::select(dplyr::all_of(names(.coefs[[1]]))) %>%
            as.matrix()

      # Coefficients will change at each step because it is an autoregressive process
      # At each step we need to add the conditional probabilities that the bird
      # was at risk and that was not at risk in the previous step
      pred <- vector(length = nrow(.sims))

      for(i in 1:nrow(.sims)){

            stepcoef1 <- stepcoef2 <- .coefs[[ind_id[i]]]
            stepcoef2[2] <- stepcoef2[2]*-1

            if(.sims[i, "ttnoon"] == -7){
                  stepcoef1[2] <- 0
                  stepcoef2[2] <- 0
                  p_t1 <- 0.5
            }

            lp1 <- .sims[i,] %*% stepcoef1
            lp2 <- .sims[i,] %*% stepcoef2

            p1 <- (exp(lp1) / (1 + exp(lp1))) * p_t1
            p2 <- (exp(lp2) / (1 + exp(lp2))) * (1-p_t1)

            pred[i] <- p_t1 <- p1+p2
      }

      # Add coordinates and risk to the sims data frame and save
      out <- cc %>%
            mutate(hgt_risk = pred)

      if(!is.null(outdir)){
            saveRDS(out, file = paste0(outdir, .col_sel$id, "_hgt_risk.rds"))
      }

      return(out)

}
