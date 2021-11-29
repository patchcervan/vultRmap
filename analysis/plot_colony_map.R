library(leaflet)
library(dplyr)
library(htmlwidgets)

rm(list = ls())


# Define directory to save the html maps to
savedir <- "../vultRmap_data_aux/risk_maps/html/"


# Load data ---------------------------------------------------------------

col_data <- read.csv("../vultRmap_data_aux/colony_data.csv")

col_data <- col_data %>%
  mutate(size = case_when(is.na(avg_ad) ~ 2,
                          avg_ad <= 20 ~ 5,
                          avg_ad > 20 & avg_ad <= 100 ~ 7,
                          avg_ad > 100 & avg_ad <= 1000 ~ 9,
                          avg_ad > 1000 ~ 11,
                          TRUE ~ 3),
         type_id = if_else(type == "roost", 0L, 1L))


# Create plot -------------------------------------------------------------


# Create a palette that maps factor levels to colors
pal <- colorFactor(c("yellow", "red", "grey"), domain = c(0, 1, 2))

# Function to modify legend circle size
addLegendCustom <- function(map, colors, labels, sizes, ...){
  colorAdditions <- paste0(colors, "; border-radius: 50%; width:", sizes, "px; height:", sizes, "px; border:1px solid black")
  labelAdditions <- paste0("<div style='display: inline-block;height: ",
                           sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>",
                           labels, "</div>")

  return(addLegend(map, colors = colorAdditions,
                   labels = labelAdditions, ...))
}

lm <- col_data %>%
  leaflet() %>%
  addProviderTiles(providers$Stamen.Terrain) %>%
  addCircleMarkers(label = col_data$name,
                   popup = paste("Name: ",col_data$name, "<br>",
                                 "Type: ", col_data$type, "<br>",
                                 "Avg_adults: ", round(col_data$avg_ad), "<br>",
                                 "Old names: ", col_data$names_old, "<br>"),
                   radius = col_data$size,
                   fillColor = pal(col_data$type_id), fillOpacity = 0.5,
                   color = "black", weight = 1, stroke = T, opacity = 1
  ) %>%
  addLegendCustom(colors = pal(c(1, 0, rep(2,5))),
                  labels = c("breed", "roost", "No count", "<20", "20-100", "100-1000", ">1000"), sizes = c(6,6,seq(6, 20, length.out = 5)),
                  opacity = 1,
                  position = "bottomright",
                  title = "Adult individuals")

saveWidget(lm, file = paste0(savedir, "colony_map.html"))
