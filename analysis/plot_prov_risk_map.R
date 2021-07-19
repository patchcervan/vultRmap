library(leaflet)
library(raster)
library(sf)
library(vultRmap)

mapdir <- "../vultRmap_data_aux/risk_maps/"

# Area to predict
SA <- readRDS("../cape_vulture_cr/data/working/gadm36_ZAF_1_sp.rds")

ec <- SA %>%
  st_as_sf() %>%
  dplyr::filter(NAME_1 == "Eastern Cape")

r <- raster::raster(paste0(mapdir, "risk_total.tif"))

r <- crop(r, ec)
r <- mask(r, ec)

# Remove 0.1% of activity to reduce mapping
cutoff <- 0.99
r[r < calcUDquantile(raster::values(r), cutoff)] <- NA

nlevels <- 10
udlevels <- seq(0, 1, length.out = nlevels + 1)

clr <- "viridis"

qpal <- colorBin(clr, raster::values(r), bins = calcUDquantile(raster::values(r), udlevels),
                 na.color = "transparent", reverse = F)

# qpal <- colorNumeric(clr, raster::values(r),
#                  na.color = "transparent", reverse = F)

labels = paste0(udlevels*100, "-", dplyr::lead(udlevels*100), " %")

leaflet() %>%
  addTiles() %>%
  addRasterImage(r, colors = qpal, opacity = 0.8) %>%
  addLegend(pal = qpal, title = "UD", values = udlevels,
            labFormat = function(type, cuts, p) {
              paste0(labels)
            })

