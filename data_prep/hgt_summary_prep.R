
# Read in the summary of the flight height model fit
hgt_fit_summary <- readRDS("../vultRmap_data_aux/hgt_fit_summ.rds")

# Save as data
usethis::use_data(hgt_fit_summary, overwrite = TRUE, version = 3)
