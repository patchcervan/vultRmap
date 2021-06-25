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

age <- "ad"


# Calculate UD for single colony ------------------------------------------



.countsdir <- countsdir
.col_id <- "cvcol373"
.age <- age
.col_sel <- col_to_pred[col_to_pred$id == "cvcol373",]

source("R/functions/calcUDFromSims.R")

# # Run these for single maps
# calcUD(.mapcode = map_codes, .rasterdir = rasterdir,
#        .outputdir = paste0(rasterdir, "2_pred_map_ud/"))
#
# calcUD(.mapcode = map_codes[9], .col_to_pred = col_to_pred, .age = age, .rasterdir = rasterdir,
#        scale = T, .outputdir = paste0(rasterdir, "2_pred_map_ud/"))

# Estimate UD without scaling
map(map_codes, ~calcUD(.mapcode = .x, .col_to_pred = col_to_pred, .age = age, scale = F,
                       .rasterdir = rasterdir, .outputdir = paste0(rasterdir, "2_pred_map_ud/")))

# Estimate UD with scaling
map(map_codes, ~calcUD(.mapcode = .x, .col_to_pred = col_to_pred, .age = age, scale = T,
                       .rasterdir = rasterdir, .outputdir = paste0(rasterdir, "2_pred_map_ud/")))



# Explore -----------------------------------------------------------------

library(leaflet)

r <- raster(paste0(rasterdir, "2_pred_map_ud/gamfit_42_19_juv.tif"))

source("R/functions/calcUDquantile.R")

nlevels <- 10
udlevels <- seq(0, 1, length.out = nlevels + 1)

qpal <- colorBin("RdYlBu", raster::values(r), bins = calcUDquantile(raster::values(r), udlevels),
                 na.color = "transparent", reverse = T)

labels = paste0(udlevels*100, "-", lead(udlevels*100), " %")

leaflet() %>%
   addTiles() %>%
   addRasterImage(r, colors = qpal, opacity = 0.8) %>%
   addLegend(pal = qpal, title = "UD", values = udlevels,
             labFormat = function(type, cuts, p) {
                paste0(labels)
             })

