library(htmlwidgets)
library(leaflet)
library(raster)
library(vultRmap)

rm(list = ls())

# Set directory where .tif maps are stored
mapdir <- "../vultRmap_data_aux/risk_maps/"

# Define maps to plot and save
maps <- c(paste0("hazard_", c("ad", "juv", "total")),
          paste0("risk_", c("ad", "juv", "total")),
          paste0("hazard_hgt_", c("ad", "juv", "total")),
          paste0("risk_hgt_", c("ad", "juv", "total")))

# Define directory to save the html maps to
savedir <- "../vultRmap_data_aux/risk_maps/html/"

# Iterate through maps and save
for(i in seq_along(maps)){

  r <- raster::raster(paste0(mapdir, maps[i], ".tif"))

  # Remove 0.1% of activity to reduce mapping
  cutoff <- 0.99
  r[r < calcUDquantile(raster::values(r), cutoff)] <- NA

  nlevels <- 10
  udlevels <- seq(0, 1, length.out = nlevels + 1)

  clr <- ifelse(length(grep("risk", maps[i])) == 1, "inferno", "magma")

  qpal <- colorBin(clr, raster::values(r), bins = calcUDquantile(raster::values(r), udlevels),
                   na.color = "transparent", reverse = TRUE)

  labels = paste0(udlevels*100, "-", dplyr::lead(udlevels*100), " %")

  lm <- leaflet() %>%
    addTiles() %>%
    addRasterImage(r, colors = qpal, opacity = 0.8) %>%
    addLegend(pal = qpal, title = "UD", values = udlevels,
              labFormat = function(type, cuts, p) {
                paste0(labels)
              })

  saveWidget(lm, file = paste0(savedir, maps[i], ".html"))

}
