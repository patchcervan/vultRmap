
# In this script we prepare 5 colonies for testing. Their location needs to
# be edited due to sensitivity of the information

library(dplyr)
set.seed(372465)

sfs_data <- read.csv("../vultRmap_data_aux/sup_feeding_data.csv")

sfs_sel <- sfs_data %>%
  rename(lon = longitude,
         lat = latitude) %>%
  filter(lon > 29 - 1,
         lon < 29 + 1,
         lat > -29 - 1,
         lat < -29 + 1) %>%
  sample_n(5)

# Modify coords
sfs_sel$lon <- sfs_sel$lon + rnorm(5, 0, 0.5)
sfs_sel$lat <- sfs_sel$lat + rnorm(5, 0, 0.5)

test_sup_feeding <- sfs_sel %>%
  mutate(id = paste0("sfs_", row_number())) %>%
  select(id, lon, lat)


# Save as data
usethis::use_data(test_sup_feeding, overwrite = TRUE, version = 3)
