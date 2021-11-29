
# In this script we fit a GAM model to the simulated activity at height risk
# for ALL colonies to smooth out the long-term distribution

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

   for(i in seq_len(nrow(col_to_pred))){

      # Select a colony to process
      col_sel <- col_to_pred[i,]

      # Load simulations
      sims <- readRDS(paste0(rasterdir, "col_hgt_sims/", col_sel$id,"_", age, "_hgt_sims.rds"))

      # Save the total number of simulated locations
      n_total <- nrow(sims)

      # Prepare covariates
      hab <- vultRmap::prepColHab(col_cc = unlist(col_sel[, c("lon", "lat")]),
                                  max_range = 1200,
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
         summarize(count = sum(hgt_risk))

      # Associate counts with covariates
      hab <- hab %>%
         mutate(lon = round(lon, 3),
                lat = round(lat, 3)) %>%
         left_join(counts, by = c("lon", "lat"))

      # Make NA counts = 0 and reduce the size of the data frame
      hab <- hab %>%
         mutate(count = if_else(is.na(count), 0, count),
                ang = calcAng(lat - col_sel$lat, lon - col_sel$lon)) %>%
         filter(dist_col < (max(.$dist_col[.$count > 0]))) %>%
         dplyr::select(count, lon, lat, ang, dist_col, log_dist_col, dist_sfs,
                       dist_col_any, elev, slope, rugg, closed, crops, urban,
                       water, prot_area)

      # Fit GAM
      print(paste("Fitting", age, i, "of", nrow(col_to_pred)))

      fit <- mgcv::bam(count ~ te(dist_col, ang, bs = c("cr", "cc")) +
                          log_dist_col + s(dist_sfs, bs = "ts", k = 4) + s(dist_col_any, bs = "ts", k = 4) +
                          s(elev, bs = "ts", k = 4) + s(slope, bs = "ts", k = 4) + s(rugg, bs = "ts", k = 4) +
                          closed+ crops+ urban+ water+ prot_area,
                       family = poisson(link = "log"), data = hab,
                       discrete = TRUE, nthreads = 10)

      expl_dev[[i]] <- 1 - fit$deviance/fit$null.deviance

      hab$gamfit <- fit$fitted.values

      # Add total number of simulated locations as an attribute
      attr(hab, "n_total") <- n_total

      # Save habitat grid with fitted values
      hab %>%
         dplyr::select(lon, lat, count, gamfit) %>%
         saveRDS(file = paste0(rasterdir, "col_hgt_gam/", col_sel$id, "_", age, "_hgt_gam.rds"))

      gc()

   }

   # Save explained deviance
   saveRDS(expl_dev, paste0("analysis/output/expl_dev_gam_", age, "_hgt.rds"))

   gc()

}
