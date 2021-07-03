#' Simulate trips from colony
#'
#' @param .nsteps Total number of steps to simulate
#' @param .age A character string indicating the age of the vulture to simulate activity for. Either 'ad' or 'juv'.
#' @param .hab Covariate values of the grid of cells representing the space the vultures can move through.
#' Each row of the data frame must represent one grid cell and must have, at least the coordinates in latitude/longitude
#' and the projected coordinates (needed for step-length calculations).
#' @param .mov_ker A named vector of parameters containing: i) the 'shape' and ii) the 'scale' of the gamma distribution
#' of step-lengths, and iii) 'model_sc', the scaling factor used for step-length when fitting the model.
#' @param .ssf_coef A vector of step-selection coefficients.
#' @param .col_sel A named vector with the coordinates (lon,lat) of the colony we want to simulate for.
#' @param .maxdist The maximum distance (in kilometers) a vulture is allowed to travel before coming back to the colony.
#' This prevents activity to accumulate at the borders of the simulation space.
#'
#' @return A data frame with the location of steps (lon, lat), distance from the central colony,
#' time of day (time-to-noon), trip number and duration of the trip (time away from colony).
#' A bird is considered to be away from the colony when it is further than 10km.
#' @export
#'
#' @examples
simTrips <- function(.nsteps, .age,  .hab, .mov_ker, .ssf_coef, .col_sel,
                     .maxdist){

  # Transform maxdist to meters
  .maxdist <- .maxdist * 1000

  # Create a data frame to store simulations
  sims <- data.frame(lon = numeric(length = .nsteps),
                     lat = numeric(length = .nsteps),
                     dist_col = numeric(length = .nsteps),
                     sl = numeric(length = .nsteps),
                     ttnoon = integer(length = .nsteps),
                     trip = integer(length = .nsteps),
                     dur = integer(length = .nsteps))

  # Correct age related predictors
  if(.age != "juv"){
    .hab <- .hab %>%
      dplyr::mutate(dplyr::across(dplyr::contains("juv"), ~.x*0))
  }

  # Predict from habitat
  X <- vector("list", 15)

  for(t in 1:15){
    ttnoon <- -8 + t
    X[[t]] <- .hab %>%
      dplyr::mutate(dplyr::across(.cols = dplyr::ends_with("ttnoon"), ~.x*ttnoon),
                    dplyr::across(.cols = dplyr::ends_with("ttnoon_sq"), ~.x*ttnoon^2)) %>%
      dplyr::select(dplyr::all_of(names(.ssf_coef))) %>%
      as.matrix() %*% .ssf_coef
  }

  # Set initial point at colony in the morning
  state <- c(.col_sel["lon"], .col_sel["lat"])
  ttnoon <- -7
  ttnoon_sq <- ttnoon^2
  dist_col <- 0
  sl <- NA_real_
  dur <- 0
  trip <- 0
  atcol <- TRUE

  # Initial location (important to keep track of projection)
  state_proj <- c(0, 0)

  # Subset to useful variables and coefficients
  .hab <- .hab %>%
    dplyr::select(lon, lat, x, y, dist_col_sc, cell_id)

  # Extract coefficients related to step-length which have been ignored until now
  sl_coefs <- .ssf_coef[grepl("sl_", names(.ssf_coef))]

  # Pre-calculate probabilities of returning to the colony
  pback <- stats::pnbinom(0:15,
                          size = vultRmap::nbinom_dur[[.age]]["size"],
                          mu = vultRmap::nbinom_dur[[.age]]["mu"])

  suppressPackageStartupMessages(
    require("dplyr") # Attach to speed up computation
  )
  require("stats")

  gc()

  for(j in 1:.nsteps){

    t <- ttnoon + 8

    # Populate simulations
    sims[j, 1] <- state[1]
    sims[j, 2] <- state[2]
    sims[j, 3] <- dist_col
    sims[j, 4] <- sl
    sims[j, 5] <- ttnoon
    sims[j, 6] <- trip
    sims[j, 7] <- dur

    # Calculate movement kernel weights (round step lengths to the nearest km)
    sls <- round(calcDist(state_proj, .hab$x, .hab$y),-3)
    sls[sls < 1000] <- 1000

    stephab <- .hab %>%
      mutate(.sls = sls,
             predh = X[[t]]) %>%
      filter(.sls < 150000) %>% # Subset space to a max step-length of 150 km to reduce computation load
      mutate(wgamma = dgamma(.sls, shape = .mov_ker["shape"], scale = .mov_ker["scale"])/(.sls*6.3/1000)) # The probability of a distance must be divided by 2*pi*distance

    # Apply model corrections for time of day (see names(sl_coefs))
    sls <- stephab$.sls/.mov_ker["model_sc"]
    sls_correct <- cbind(sls, sls*ttnoon, sls*ttnoon_sq, sls, sls*(.age=="juv")) %*% sl_coefs

    # Calculate step sampling weights multiplying habitat and movement kernels
    stephab <- stephab %>%
      mutate(predh = exp(predh + sls_correct),
             w = predh*wgamma)

    # Sample one location
    step <- slice_sample(stephab, n = 1, weight_by = w)

    # Update state and time
    if(step$dist_col_sc > .maxdist){

      state[1] <- .col_sel["lon"]
      state[2] <- .col_sel["lat"]
      state_proj <- c(0,0)
      sl <- NA_real_
      dur <- 0
      dist_col <- 0
      atcol <- TRUE

    } else {

      state[1] <- step$lon
      state[2] <- step$lat
      state_proj[1] <- step$x
      state_proj[2] <- step$y # Note that we need a state in projected coordinates
      dist_col <- step$dist_col_sc
      sl <- step$.sls
      ttnoon <- ttnoon + 1

      if(dist_col > 5000){
        dur <- dur + 1

      } else {
        dur <- 0
        atcol <- TRUE
      }

      if(dur == 1){
        trip <- trip + 1
      }

    }

    if(ttnoon > 7){

      ttnoon <- -7

      if(!atcol && stats::rbinom(1, 1, pback[floor(dur/15)])){
        state[1] <- .col_sel["lon"]
        state[2] <- .col_sel["lat"]
        state_proj <- c(0,0)
        dur <- 0
        dist_col <- 0
        sl <- NA_real_
      }

      atcol <- FALSE
    }

    ttnoon_sq <- ttnoon^2

    # hab %>%
    #    ggplot() +
    #    geom_raster(aes(lon, lat, fill = w))
    # ggsave(paste("figures/step_weights_", j-1, ".png" ))

  }

  detach("package:dplyr", unload = TRUE)

  return(sims)
}
