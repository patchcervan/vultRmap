
# Read in movement kernel in RDS format imported from cape_vulture_cr project
mov_kernel <- readRDS("../vultRmap_data_aux/gamma_kern.rds")

# Save as data
usethis::use_data(mov_kernel, overwrite = TRUE, version = 3)
