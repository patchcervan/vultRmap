# 22-02-2021

# In this script we estimate an utilization distribution within a defined area
# based on selection values estimated using an SSF model

rm(list = ls())

library(vultRmap)


# Load data and model results ---------------------------------------------

# Colony and roost data
colony_all <- read.csv("../vultRmap_data_aux/colony_data.csv")

# Define directory with smoothed counts
countsdir <- "../vultRmap_data_aux/col_gam/"


# Select target colonies ---------------------------------------------------

# Subset those colonies and roosts for which we have data.
# Further subset roosts with more than 50 birds
col_to_pred <- colony_all %>%
   dplyr::filter(!is.na(avg_ad)) %>%
   dplyr::filter((type == "breed" & avg_ad > 0) | (type == "roost" & (avg_ad + avg_juv) > 50))


# Define age --------------------------------------------------------------

age <- "juv"


# Calculate hazard for single colony ------------------------------------------

col_sel <- col_to_pred[col_to_pred$id == "cvcol379",]

ud <- calcUdColony(.col_sel = col_sel, .age = age, .scale = FALSE,
                .countsdir = countsdir, .outputdir = NULL)

# Make raster file
# r_count <- ud %>%
#    dplyr::select(x = lon, y = lat, z = count) %>%
#    raster::rasterFromXYZ(crs = sp::CRS("+init=epsg:4326"))

r_gamfit <- ud %>%
   dplyr::select(x = lon, y = lat, z = gamfit) %>%
   raster::rasterFromXYZ(crs = sp::CRS("+init=epsg:4326"))

# Save raster file
# raster::writeRaster(r_count, paste0("analysis/output/count_",
#                                      col_sel$id, "_", age, ".tif"))

raster::writeRaster(r_gamfit, paste0("analysis/output/enc_hazard_",
                                     col_sel$id, "_", age, ".tif"),
                    overwrite = TRUE)


# Calculate hazard for all colonies -------------------------------------------

hab <- vultRmap::range_covts %>%
   dplyr::select(lon, lat)

attr(hab, "mod_scale") <- NULL

ud <- calcUdColony(.col_sel = col_to_pred[1,], .hab = hab, .age = age,
                .scale = FALSE, .countsdir = countsdir, .outputdir = NULL)

for(i in seq_len(nrow(col_to_pred[-1,]))){

   newud <- calcUdColony(.col_sel = col_to_pred[i,], .hab = hab, .age = age,
                   .scale = FALSE, .countsdir = countsdir, .outputdir = NULL)

   ud$count <- ud$count + newud$count
   ud$gamfit <- ud$gamfit + newud$gamfit

}

# Make raster file
# r_count <- ud %>%
#    dplyr::select(x = lon, y = lat, z = count) %>%
#    raster::rasterFromXYZ(crs = sp::CRS("+init=epsg:4326"))

r_gamfit <- ud %>%
   dplyr::select(x = lon, y = lat, z = gamfit) %>%
   raster::rasterFromXYZ(crs = sp::CRS("+init=epsg:4326"))

# Save raster file
raster::writeRaster(r_gamfit,
                    paste0("analysis/output/enc_hazard_", age, ".tif"),
                    overwrite = TRUE)


# Calculate risk for all colonies -------------------------------------------

hab <- vultRmap::range_covts %>%
   dplyr::select(lon, lat)

attr(hab, "mod_scale") <- NULL

ud <- calcUdColony(.col_sel = col_to_pred[1,], .hab = hab, .age = age,
                   .scale = TRUE, .countsdir = countsdir, .outputdir = NULL)

for(i in seq_len(nrow(col_to_pred[-1,]))){

   newud <- calcUdColony(.col_sel = col_to_pred[i,], .hab = hab, .age = age,
                         .scale = TRUE, .countsdir = countsdir, .outputdir = NULL)

   ud$count <- ud$count + newud$count
   ud$gamfit <- ud$gamfit + newud$gamfit

}

# Make raster file
# r_count <- ud %>%
#    dplyr::select(x = lon, y = lat, z = count) %>%
#    raster::rasterFromXYZ(crs = sp::CRS("+init=epsg:4326"))

r_gamfit <- ud %>%
   dplyr::select(x = lon, y = lat, z = gamfit) %>%
   raster::rasterFromXYZ(crs = sp::CRS("+init=epsg:4326"))

# Save raster file
raster::writeRaster(r_gamfit,
                    paste0("analysis/output/enc_risk_", age, ".tif"),
                    overwrite = TRUE)


# Explore -----------------------------------------------------------------

library(leaflet)
library(raster)

r <- raster::raster("analysis/output/enc_risk_juv.tif")

nlevels <- 10
udlevels <- seq(0, 1, length.out = nlevels + 1)

qpal <- colorBin("RdYlBu", raster::values(r), bins = calcUDquantile(raster::values(r), udlevels),
                 na.color = "transparent", reverse = T)

labels = paste0(udlevels*100, "-", dplyr::lead(udlevels*100), " %")

leaflet() %>%
   addTiles() %>%
   addRasterImage(r, colors = qpal, opacity = 0.8) %>%
   addLegend(pal = qpal, title = "UD", values = udlevels,
             labFormat = function(type, cuts, p) {
                paste0(labels)
             })

