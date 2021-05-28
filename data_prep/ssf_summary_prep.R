
# Read in movement kernel in RDS format imported from cape_vulture_cr project
ssf_fit_summary <- readRDS("data_aux/ssf_fit_summary.rds")

# Save as data
usethis::use_data(ssf_fit_summary, overwrite = TRUE, version = 3)
