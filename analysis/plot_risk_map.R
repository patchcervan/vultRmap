library(leaflet)
library(raster)
library(vultRmap)

r <- raster::raster("analysis/output/risk_maps/risk_total.tif")

#+ raster::raster("../vultRmap_data_aux/risk_maps/enc_hgt_risk_juv.tif")

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


# Save a categorized map -------------------------------------------------


