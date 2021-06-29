
rm(list = ls())

# Prepare covariates ------------------------------------------------------

# Load covariates
range_covts <- readRDS("../vultRmap_data_aux/df_hab_general.rds")


# Prepare scaling factors -------------------------------------------------

# SSF model fit
ssf_fit <- readRDS("../vultRmap_data_aux/ssf_fit_10pp.rds")
sds <- unlist(sapply(ssf_fit$frame, attr, "scaled:scale"))
attr(range_covts, "ssf_mod_scale") <- sds

rm(ssf_fit)

# Height model fit
hgt_fit <- readRDS("../vultRmap_data_aux/height_fit.rds")
sds <- unlist(sapply(hgt_fit$frame, attr, "scaled:scale"))
attr(range_covts, "hgt_mod_scale") <- sds

rm(hgt_fit)

# Save data ---------------------------------------------------------------

# Save as data
usethis::use_data(range_covts, overwrite = TRUE, compress = "xz", version = 3)
