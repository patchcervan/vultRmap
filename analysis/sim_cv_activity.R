library(vultRmap)

rm(list = ls())


# Simulate for one colony -------------------------------------------------

# Load colony data to find a colony
col_all <- read.csv("../vultRmap_data_aux/colony_data.csv")

system.time(
  sims <- vultRmap::simOneColony(age = "juv",
                                 totalsteps = 1000,
                                 ncores = 5,
                                 col_sel = unlist(col_all[303, c("lon", "lat")]),
                                 set_seed = round(runif(1, 1, 1e5)),
                                 dist_lim = 1000,
                                 sample_coefs = 5,
                                 data_dir = "../vultRmap_data_aux")
)

library(ggplot2)

sfs <- read.csv("../vultRmap_data_aux/sup_feeding_data.csv")

ssf_coef <- vultRmap::sampleSsfCoef(nind = 1)

# This may take some minutes if max_range is large (>500)
hab <- vultRmap::prepColHab(col_cc = unlist(col_all[303, c("lon", "lat")]), max_range = 1000,
                            col_all = col_all, sfs = sfs)

hab <- vultRmap::completeDataFrame(hab, names(ssf_coef[[1]]), 0)

ggplot(hab) +
  geom_raster(aes(x = lon, y = lat, fill = dist_col_sc)) +
  # geom_tile(data = hab[hab$dist_col_sc > (1000 - 1000*10/100)*1000,], aes(x = lon, y = lat), col = "red") +
  coord_equal()

ggplot(sims) +
  geom_raster(data = hab, aes(x = lon, y = lat, fill = dist_col_sc)) +
  geom_point(aes(x = lon, y = lat)) +
  geom_density2d(aes(x = lon, y = lat)) +
  geom_tile(data = hab[hab$dist_col_sc > (1000 - 1000*10/100)*1000,], aes(x = lon, y = lat), col = "red") +
  coord_equal()


# Simulate for all colonies -----------------------------------------------

system.time(
  vultRmap::simAllColonies(min_size_col = 2000, min_size_roost = 100,
                           age = "juv",
                           totalsteps = 1000,
                           ncores = 5,
                           set_seed = round(runif(1, 1, 1e5)),
                           dist_lim = 1000,
                           sample_coefs = 5,
                           out_dir = "analysis/output",
                           data_dir = "../vultRmap_data_aux")
)

library(ggplot2)

# Load colony data to find a colony
col_all <- read.csv("../vultRmap_data_aux/colony_data.csv")

# Supplementary feeding sites
sfs <- read.csv("../vultRmap_data_aux/sup_feeding_data.csv")

ssf_coef <- vultRmap::sampleSsfCoef(nind = 1)

# List files in output folder
ff <- list.files("analysis/output")
ids <- sub(".rds", "", ff)

i <- 4

# What colony
col_sel <- col_all[col_all$id == ids[i],]

# Load sims
sims <- readRDS(paste0("analysis/output/", ff[i]))

# This may take some minutes if max_range is large (>500)
hab <- vultRmap::prepColHab(col_cc = unlist(col_sel[, c("lon", "lat")]), max_range = 1000,
                            col_all = col_all, sfs = sfs)

hab <- vultRmap::completeDataFrame(hab, names(ssf_coef[[1]]), 0)

ggplot(sims) +
  geom_raster(data = hab, aes(x = lon, y = lat, fill = dist_col_sc)) +
  geom_point(aes(x = lon, y = lat)) +
  geom_density2d(aes(x = lon, y = lat)) +
  # geom_tile(data = hab[hab$dist_col_sc > (1000 - 1000*10/100)*1000,], aes(x = lon, y = lat), col = "red") +
  coord_equal()
