# 16-07-2021

# In this script we build the hazard and risk maps for all colonies together
# both in 2D and 3D.


# xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# MAKE 2D RISK MAPS ---------------------------------------
# xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx


library(vultRmap)

rm(list = ls())


# Load data and model results ---------------------------------------------

# Colony and roost data
colony_all <- read.csv("../vultRmap_data_aux/colony_data.csv")

# Define directory with smoothed counts
countsdir <- "../vultRmap_data_aux/col_gam/"

# Define a directory where maps should be saved to
savedir <- "../vultRmap_data_aux/risk_maps/"


# Select target colonies ---------------------------------------------------

# Subset those colonies and roosts for which we have data.
# Further subset roosts with more than 50 birds
col_to_pred <- colony_all %>%
  dplyr::filter(!is.na(avg_ad)) %>%
  dplyr::filter((type == "breed" & avg_ad > 0) | (type == "roost" & (avg_ad + avg_juv) >= 50))


# Calculate hazard for all colonies -------------------------------------------

ages <- c("ad", "juv")

for(j in 1:2){

  age <- ages[j]

  make2DRiskMap(col_to_pred = col_to_pred, age = age, map_type = "hazard",
                countsdir = countsdir,
                suffix = paste0("_", age, "_gam.rds"),
                outdir = savedir)

}


# Calculate risk for all colonies -------------------------------------------

ages <- c("ad", "juv")

hab <- vultRmap::range_covts %>%
  dplyr::select(lon, lat)

attr(hab, "mod_scale") <- NULL

for(j in 1:2){

  age <- ages[j]

  make2DRiskMap(col_to_pred = col_to_pred, age = age, map_type = "risk",
                countsdir = countsdir,
                suffix = paste0("_", age, "_gam.rds"),
                outdir = savedir)

}


# Calculate 2D risk totals --------------------------------------------------------

# Hazard totals
rr <- raster::raster(paste0(savedir, "hazard_ad.tif")) +
  raster::raster(paste0(savedir, "hazard_juv.tif"))

raster::writeRaster(rr, paste0(savedir, "hazard_total.tif"), overwrite = TRUE)

# Risk totals
rr <- raster::raster(paste0(savedir, "risk_ad.tif")) +
  raster::raster(paste0(savedir, "risk_juv.tif"))

raster::writeRaster(rr, paste0(savedir, "risk_total.tif"), overwrite = TRUE)



# xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# MAKE 3D RISK MAPS ---------------------------------------
# xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx


library(vultRmap)

rm(list = ls())


# Load data and model results ---------------------------------------------

# Colony and roost data
colony_all <- read.csv("../vultRmap_data_aux/colony_data.csv")

# Define directory with smoothed counts
countsdir <- "../vultRmap_data_aux/col_hgt_gam/"

# Define a directory where maps should be saved to
savedir <- "../vultRmap_data_aux/risk_maps/"


# Select target colonies ---------------------------------------------------

# Subset those colonies and roosts for which we have data.
# Further subset roosts with more than 50 birds
col_to_pred <- colony_all %>%
  dplyr::filter(!is.na(avg_ad)) %>%
  dplyr::filter((type == "breed" & avg_ad > 0) | (type == "roost" & (avg_ad + avg_juv) >= 50))


# Calculate hazard for all colonies -------------------------------------------

ages <- c("ad", "juv")

for(j in 1:2){

  age <- ages[j]

  make3DRiskMap(col_to_pred = col_to_pred, age = age, map_type = "hazard",
                countsdir = countsdir,
                suffix = paste0("_", age, "_hgt_gam.rds"),
                outdir = savedir)

}


# Calculate risk for all colonies -------------------------------------------

ages <- c("ad", "juv")

hab <- vultRmap::range_covts %>%
  dplyr::select(lon, lat)

attr(hab, "mod_scale") <- NULL

for(j in 1:2){

  age <- ages[j]

  make3DRiskMap(col_to_pred = col_to_pred, age = age, map_type = "risk",
                countsdir = countsdir,
                suffix = paste0("_", age, "_hgt_gam.rds"),
                outdir = savedir)

}


# Calculate 3D risk totals --------------------------------------------------------

# Hazard totals
rr <- raster::raster(paste0(savedir, "hazard_hgt_ad.tif")) +
  raster::raster(paste0(savedir, "hazard_hgt_juv.tif"))

raster::writeRaster(rr, paste0(savedir, "hazard_hgt_total.tif"), overwrite = TRUE)

# Risk totals
rr <- raster::raster(paste0(savedir, "risk_hgt_ad.tif")) +
  raster::raster(paste0(savedir, "risk_hgt_juv.tif"))

raster::writeRaster(rr, paste0(savedir, "risk_hgt_total.tif"), overwrite = TRUE)
