# 22-02-2021

# In this script we estimate the utilization distribution at rotor height
# either for a single colony or for the whole range using all colonies

rm(list = ls())

library(vultRmap)


# Load data and model results ---------------------------------------------

# Colony and roost data
colony_all <- read.csv("../vultRmap_data_aux/colony_data.csv")

# Define directory with smoothed counts
countsdir <- "../vultRmap_data_aux/col_hgt_gam/"


# Select target colonies ---------------------------------------------------

# Subset those colonies and roosts for which we have data.
# Further subset roosts with more than 50 birds
col_to_pred <- colony_all %>%
   dplyr::filter(!is.na(avg_ad)) %>%
   dplyr::filter((type == "breed" & avg_ad > 0) | (type == "roost" & (avg_ad + avg_juv) > 50))


# Calculate hazard for single colony ------------------------------------------

# Define age
age <- "ad"

# Define colony
col_sel <- col_to_pred[col_to_pred$id == "cvcol394",]

# Calculate 3D hazard (UD) with .scale = FALSE or risk with .scale = TRUE
ud <- calcUdHgtColony(.col_sel = col_sel, .age = age, .scale = TRUE,
                      .countsdir = countsdir, .outputdir = NULL)

# Make raster file
r_gamfit <- ud %>%
   dplyr::select(x = lon, y = lat, z = gamfit) %>%
   raster::rasterFromXYZ(crs = sp::CRS("+init=epsg:4326"))

# Save raster file
raster::writeRaster(r_gamfit, paste0("analysis/output/risk_maps/risk_hgt_",
                                     col_sel$id, "_", age, ".tif"),
                    overwrite = TRUE)


# Calculate hazard for all colonies -------------------------------------------

ages <- c("ad", "juv")

hab <- vultRmap::range_covts %>%
   dplyr::select(lon, lat)

attr(hab, "mod_scale") <- NULL

for(j in 1:2){

   age <- ages[j]

   make3DRiskMap(col_to_pred = col_to_pred, age = age, map_type = "hazard",
                 countsdir = "../vultRmap_data_aux/col_hgt_gam/",
                 outdir = "analysis/output/risk_maps/")

}


# Calculate risk for all colonies -------------------------------------------

ages <- c("ad", "juv")

hab <- vultRmap::range_covts %>%
   dplyr::select(lon, lat)

attr(hab, "mod_scale") <- NULL

for(j in 1:2){

   age <- ages[j]

   make3DRiskMap(col_to_pred = col_to_pred, age = age, map_type = "risk",
                 countsdir = "../vultRmap_data_aux/col_hgt_gam/",
                 outdir = "analysis/output/risk_maps/")

}


# Calculate totals --------------------------------------------------------

# Hazard totals
rr <- raster::raster("analysis/output/risk_maps/hazard_hgt_ad.tif") +
   raster::raster("analysis/output/risk_maps/hazard_hgt_juv.tif")

raster::writeRaster(rr, "analysis/output/risk_maps/hazard_hgt_total.tif", overwrite = TRUE)

# Risk totals
rr <- raster::raster("analysis/output/risk_maps/risk_hgt_ad.tif") +
   raster::raster("analysis/output/risk_maps/risk_hgt_juv.tif")

raster::writeRaster(rr, "analysis/output/risk_maps/risk_hgt_total.tif", overwrite = TRUE)
