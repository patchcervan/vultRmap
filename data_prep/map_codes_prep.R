rm(list = ls())

map_codes <- c("40_17", "40_18", "40_19", "41_17", "41_18", "41_19", "42_17",
               "42_18", "42_19", "43_17", "43_18", "43_19")

# Save data ---------------------------------------------------------------

# Save as data
usethis::use_data(map_codes, overwrite = TRUE, compress = "xz", version = 3)
