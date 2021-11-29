# 22-02-2021

# In this script we estimate the utilization distribution either for a single
# colony or for the whole range using all colonies

rm(list = ls())

library(vultRmap)


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

# Define colony
col_sel <- col_to_pred[col_to_pred$id == "cvcol379",]


# Calculate hazard for single colony ------------------------------------------

ages <- c("ad", "juv")

for(j in 1:2){

   age <- ages[j]

   # Calculate hazard (UD) with .scale = FALSE or risk with .scale = TRUE
   ud <- calcUdColony(.col_sel = col_sel, .age = age, .scale = FALSE,
                      .countsdir = countsdir,
                      .suffix = paste0("_", age, "_gam.rds"),
                      .outputdir = NULL)

   # Make raster file
   r_gamfit <- ud %>%
      dplyr::select(x = lon, y = lat, z = gamfit) %>%
      raster::rasterFromXYZ(crs = sp::CRS("+init=epsg:4326"))

   # Save raster file
   raster::writeRaster(r_gamfit, paste0("analysis/output/risk_maps/hazard_",
                                        col_sel$id, "_", age, ".tif"),
                       overwrite = TRUE)

}


# Calculate risk for single colony ------------------------------------------

ages <- c("ad", "juv")

for(j in 1:2){

   age <- ages[j]

   # Calculate hazard (UD) with .scale = FALSE or risk with .scale = TRUE
   ud <- calcUdColony(.col_sel = col_sel, .age = age, .scale = TRUE,
                      .countsdir = countsdir,
                      .suffix = paste0("_", age, "_gam.rds"),
                      .outputdir = NULL)

   # Make raster file
   r_gamfit <- ud %>%
      dplyr::select(x = lon, y = lat, z = gamfit) %>%
      raster::rasterFromXYZ(crs = sp::CRS("+init=epsg:4326"))

   # Save raster file
   raster::writeRaster(r_gamfit, paste0("analysis/output/risk_maps/risk_",
                                        col_sel$id, "_", age, ".tif"),
                       overwrite = TRUE)

}
