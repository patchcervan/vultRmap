

# Prepare covariates ------------------------------------------------------

# Load covariates
range_covts <- readRDS("../vultRmap_data_aux/df_hab_general.rds")

# Clean useless columns
# identical(range_covts$y, range_covts$lat)
# identical(round(range_covts$y, 3), range_covts$lat)

range_covts <- range_covts %>%
  dplyr::select(!dplyr::contains("dist")) %>%
  dplyr::select(!dplyr::contains("sl_")) %>%
  dplyr::select(!dplyr::all_of(c("x", "y")))


# Prepare scaling factors -------------------------------------------------

# Model fit
ssf_fit <- readRDS("../vultRmap_data_aux/ssf_fit_dist_tnoon.rds")

sds <- unlist(sapply(ssf_fit$frame, attr, "scaled:scale"))

attr(range_covts, "mod_scale") <- sds

rm(ssf_fit)


# Save data ---------------------------------------------------------------

# Save as data
usethis::use_data(range_covts, overwrite = TRUE, compress = "xz", version = 3)

rm(range_covts, sds)
