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

# Save buffer
write_sf(col_sel_buff, "../vultRmap_data_aux/ec_col_buff.shp")


# Iterate through maps and save -------------------------------------------

for(i in seq_along(maps)){

  r <- raster::raster(paste0(mapdir, maps[i], ".tif"))

  # Make a binned raster with 10 categories discarding the top 1%
  # r <- focal(r, w = matrix(1, 5, 5), mean, na.rm = T)
  y <- makeBinnedRasterMap(r, nlevels = 10, cutoff = 0.99)
  y <- as.factor(y)

  # Crop map
  frame <- extent(c(15, 40, -37, -17))
  y <- crop(y, frame)

  # Prepare colour palette
  clr <- ifelse(length(grep("risk", maps[i])) == 1, "inferno", "magma")

  qpal <- colorFactor(clr, levels(y)[[1]][,1,drop = TRUE], #bins = bins,
                      na.color = "#00000000", reverse = FALSE)

  lm <- leaflet() %>%
    addTiles() %>%
    addRasterImage(y, colors = qpal, opacity = 0.8) %>%
    addPolygons(data = col_sel_buff, color = "#444444", weight = 1.2,
                opacity = 1.0, fillOpacity = 0.2) %>%
    addLegend("topright", pal = qpal, values = round(unique(getValues(y)), 1),
              title = "UD",
              labFormat = labelFormat(suffix = "%",
                                      transform = function(x) 100 * x),
              opacity = 0.8)

  saveWidget(lm, file = paste0(savedir, maps[i], ".html"))

}
