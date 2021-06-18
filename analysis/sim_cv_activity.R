library(vultRmap)

rm(list = ls())


# Read in necessary data --------------------------------------------------

# We will need to calculate distance to other colonies
col_all <- read.csv("../vultRmap_data_aux/colony_data.csv")

# And to supplementary feeding sites
sfs <- read.csv("../vultRmap_data_aux/sup_feeding_data.csv")

# If you are updating the map, consider if these files need to be updated too!

# Subset those colonies and roosts we have data for.
# Further subset roosts with more than 50 birds
col_to_pred <- col_all %>%
  dplyr::filter(!is.na(avg_ad)) %>%
  dplyr::filter((type == "breed" & avg_ad > 0) | (type == "roost" & (avg_ad + avg_juv) > 50))


# Define simulation parameters --------------------------------------------

# Age
age <- "juv"

# Define number of steps for the simulation
totalsteps <- 10000

# Number of steps per core
ncores <- 10
nsteps <- rep(ceiling(totalsteps/ncores), ncores) # rounded up, so that we get AT LEAST the desired number of steps

# Sample random coefficients for each core
ssf_coef <- sampleSsfCoef(nind = ncores, seed = 3487)

# Select a colony
col_sel <- unlist(col_to_pred[8, c("lon", "lat")])


# Prepare habitat for simulations -----------------------------------------

# This may take some minutes if max_range is large (>500)
system.time(
  hab <- vultRmap::prepColHab(col_cc = col_sel, max_range = 1100,
                              col_all = col_all, sfs = sfs)
)

hab <- vultRmap::completeDataFrame(hab, names(ssf_coef[[1]]), 0)

# Reduce space to reduce computation time?
hab <- hab %>%
  dplyr::filter(dist_col < 4)


# Simulate activity -------------------------------------------------------

# Configure multicore
future::plan("multisession", workers = ncores)

# Simulate
system.time(
  sims <- furrr::future_map2_dfr(nsteps, ssf_coef,
                                 ~vultRmap::simTrips(.nsteps = .x, .ssf_coef = .y,
                                                     .age = age, .hab = hab,
                                                     .mov_ker = vultRmap::mov_kernel,
                                                     .col_sel = col_sel,
                                                     .maxdist = max(hab$dist_col_sc) - 200000),
                                 .options = furrr::furrr_options(seed = 35563))
)

future::plan("sequential")

library(ggplot2)

ggplot(sims) +
  geom_raster(data = hab, aes(x = lon, y = lat, fill = dist_col_sc)) +
  geom_point(aes(x = lon, y = lat)) +
  geom_density2d(aes(x = lon, y = lat)) +
  # geom_tile(data = hab[hab$dist_col_sc > 8.8e5,], aes(x = lon, y = lat), col = "red") +
  coord_equal()

head(sims)
sims

max(sims$dist_col)

distsprime <- calcDist(col_sel, sims$lon, sims$lat)
head(distsprime)
head(distsprime*111000)

hab %>%
  dplyr::filter(cell_id < 100) %>%
  ggplot() +
  geom_raster(aes(x = lon, y = lat, fill = cell_id)) +
  geom_text(aes(x = lon, y = lat, label = cell_id), col = "red") +
  coord_equal()
