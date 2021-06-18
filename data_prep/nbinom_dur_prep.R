
# Tracking data
vults <- readRDS("../vultRmap_data_aux/data_ssf_ready_10pp.rds")

# Find a parametric distribution for trip duration (neg binomial) for each
# age class
trip_dur <- vults %>%
  dplyr::group_by(age_fct, trip) %>%
  dplyr::summarize(dur = max(days_away))

nbinom_dur <- list(ad = fitdistrplus::fitdist(trip_dur$dur[trip_dur$age_fct == "ad"], distr = "nbinom")$estimate,
                   juv = fitdistrplus::fitdist(trip_dur$dur[trip_dur$age_fct == "juv"], distr = "nbinom")$estimate)

# Save as data
usethis::use_data(nbinom_dur, overwrite = TRUE, version = 3)
