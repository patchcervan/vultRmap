
# In this script we fit a GAM model to the simulated activity to smooth out
# the long-term distribution

library(tidyverse)
library(mgcv)
library(vultRmap)

rm(list = ls())

# Define raster output directory
rasterdir <- "../vultRmap_data_aux/"


# Read in data ------------------------------------------------------------

# Colony data
col_all <- read_csv("../vultRmap_data_aux/colony_data.csv")

# Supplementary feeding sites
sfs <- read_csv("../vultRmap_data_aux/sup_feeding_data.csv")

# Subset those colonies and roosts we have data for.
col_to_pred <- col_all %>%
   filter(!is.na(avg_ad)) %>%
   filter((type == "breed" & avg_ad > 0) | (type == "roost" & (avg_ad + avg_juv) > 50))


# Loop through colonies and fit GAM ---------------------------------------

ages <- c("ad", "juv")

for(j in 1:2){

   # Object to store explained deviance values
   expl_dev <- vector(length = nrow(col_to_pred))

   # Define age
   age <- ages[j]

   for(i in 1:nrow(col_to_pred)){

      # Select a colony to process
      col_sel <- col_to_pred[i,]

      # Load simulations
      sims <- readRDS(paste0(rasterdir, "col_sims/", col_sel$id, "_", age, "_sims.rds"))

      # If we have two different simulation files that we want to join
      try({
         sims <- rbind(sims,
                       readRDS(paste0(rasterdir, "col_sims/", col_sel$id, "_", age, "_sims_v2.rds")))
      })

      # Prepare covariates
      hab <- vultRmap::prepColHab(col_cc = unlist(col_sel[, c("lon", "lat")]),
                                  max_range = max(sims$dist_col)/1000 + 10,
                                  col_all = col_all, sfs = sfs)

      # Fix locations at colony, as it may not correspond to any cell centroid.
      # Move to closest location
      col_grid <- hab %>%
         dplyr::filter(x == 0 & y == 0) %>%
         dplyr::select(lon, lat) %>%
         unlist()

      sims <- sims %>%
         dplyr::mutate(lon = ifelse(lon == col_sel$lon, col_grid[1], lon),
                       lat = ifelse(lat == col_sel$lat, col_grid[2], lat))

      # Count the number of visits to each cell
      counts <- sims %>%
        mutate(lon = round(lon, 3),
               lat = round(lat, 3)) %>%
        group_by(lon, lat) %>%
        summarize(count = n()) %>%
        ungroup()

      # Associate counts with covariates
      hab <- hab %>%
         mutate(lon = round(lon, 3),
                lat = round(lat, 3)) %>%
         left_join(counts, by = c("lon", "lat"))

      # Make NA counts = 0 and reduce the size of the data frame
      hab <- hab %>%
         mutate(count = if_else(is.na(count), 0L, count),
                ang = calcAng(lat - col_sel$lat, lon - col_sel$lon)) %>%
         filter(dist_col < (max(.$dist_col[.$count > 0]))) %>%
         dplyr::select(count, lon, lat, ang, dist_col, log_dist_col, dist_sfs,
                       dist_col_any, elev, slope, rugg, closed, crops, urban,
                       water, prot_area)

      hab <- hab %>%
        mutate(log_dist_sfs = log(dist_sfs + 1))

      # Fit GAM
      print(paste("Fitting", age, i, "of", nrow(col_to_pred)))

      fit <- mgcv::bam(count ~ te(log_dist_col, ang, bs = c("cr", "cc")) +
                          dist_sfs + dist_col_any +
                          s(elev, bs = "ts", k = 4) + s(slope, bs = "ts", k = 4) + s(rugg, bs = "ts", k = 4) +
                          closed+ crops+ urban+ water+ prot_area,
                       family = poisson(link = "log"), data = hab,
                       discrete = TRUE, nthreads = 10)

      expl_dev[[i]] <- 1 - fit$deviance/fit$null.deviance

      hab$gamfit <- fit$fitted.values

      # Save habitat grid with fitted values
      hab %>%
         dplyr::select(lon, lat, count, gamfit) %>%
         saveRDS(file = paste0(rasterdir, "col_gam/", col_sel$id, "_", age, "_gam.rds"))

      gc()

   }

   # Save explained deviance
   saveRDS(expl_dev, paste0("analysis/output/expl_deviance/expl_dev_gam_log_", age, ".rds"))

   gc()

}
