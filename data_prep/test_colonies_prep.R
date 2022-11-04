
# In this script we prepare 5 colonies for testing. Their location needs to
# be edited due to sensitivity of the information

library(dplyr)
set.seed(372465)

col_data <- read.csv("../vultRmap_data_aux/colony_data.csv")

col_sel <- col_data %>%
  filter(lon > 29 - 1,
         lon < 29 + 1,
         lat > -29 - 1,
         lat < -29 + 1) %>%
  sample_n(5)

# Modify coords
col_sel$lon <- col_sel$lon + rnorm(5, -1, 1)
col_sel$lat <- col_sel$lat + rnorm(5, 1, 1)

test_colonies <- col_sel %>%
  select(id, lon, lat) %>%
  mutate(id = paste0("test_", row_number()))

# Save as data
usethis::use_data(test_colonies, overwrite = TRUE, version = 3)
