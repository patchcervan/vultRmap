library(htmlwidgets)
library(leaflet)
library(raster)
library(sf)
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


# Prepare buffer around EC colonies ---------------------------------------

# Load colony data
colony_all <- read.csv("../vultRmap_data_aux/colony_data.csv")

# Filter those colonies we are interested in
col_sel <- colony_all %>%
  dplyr::filter(id %in% c("da_32", "da_65", "da_201", "da_75", "da_24",
                          "da_9", "da_200", "da_38", "da_27", "da_239",
                          "da_4", "cvcol669", "cvco719", "cvcol687", "cvcol589")) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Create a 50 kilometer buffer
sf_use_s2(FALSE)
col_sel_buff <- col_sel %>%
  st_combine() %>%
  st_convex_hull() %>%
  st_sf() %>%
  st_buffer(dist = 0.4)

col_sel_buff <- col_sel_buff %>%
  st_union() %>%
  st_sf()


# Iterate through maps and save -------------------------------------------

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
    addPolygons(data = col_sel_buff, color = "#444444", weight = 1.2,
                opacity = 1.0, fillOpacity = 0.2) %>%
    addLegend(pal = qpal, title = "UD", values = udlevels,
              labFormat = function(type, cuts, p) {
                paste0(labels)
              })

  saveWidget(lm, file = paste0(savedir, maps[i], ".html"))

}
