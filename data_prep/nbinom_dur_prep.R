library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)

rm(list = ls())


# Tracking data
vults <- readRDS("../vultRmap_data_aux/data_ssf_ready_10pp.rds")


# Define trips from colony ------------------------------------------------

# Each day will be a trip unless the bird doesn't return to colony (dist_col < 5e3)

# Number of locations for each bird
vults <- vults %>%
  filter(case_ == TRUE) %>%
  dplyr::select(bird_id, age_fct, t2_, date, dist_col) %>%
  nest_by(bird_id)

# Function to define trips from the central colony
f <- function(trk, min_dist = 5e3){

  trk <- trk %>%
    mutate(date = lubridate::date(t2_))

  trips <- trk %>%
    group_by(date) %>%
    summarize(mindist = min(dist_col)) %>%
    mutate(at_col = if_else(mindist <= min_dist, 1, 0))

  trips$trip <- trips$at_col

  # Initiate trips
  t <- 1
  trips$trip[1] <- t

  for(i in 2:nrow(trips)){
    if(trips$trip[i] == 0){
      trips$trip[i] <- t
    } else {
      t <- t + 1
      trips$trip[i] <- t
    }
  }

  trips <- trips %>%
    group_by(trip) %>%
    mutate(days_away = difftime(date, lag(date), units = "day"),
           days_away = ifelse(is.na(days_away), 0, days_away),
           days_away = cumsum(days_away)) %>%
    ungroup()

  trk <- trk %>%
    left_join(trips %>% dplyr::select(date, trip, days_away),
              by = "date")

  return(trk)

}

vults$data2 <- vults$data %>%
  map(~f(.x, 2e3))

vults$data5 <- vults$data %>%
  map(~f(.x, 5e3))

vults$data10 <- vults$data %>%
  map(~f(.x, 10e3))



# Prepare output data -----------------------------------------------------

# For min 2km
trip_dur <- vults %>%
  dplyr::select(bird_id, data2) %>%
  unnest(cols = c(bird_id, data2)) %>%
  mutate(trip = paste(bird_id, trip, sep = "_")) %>%
  dplyr::group_by(age_fct, trip) %>%
  dplyr::summarize(dur = max(days_away))

# Find a parametric distribution for trip duration (neg binomial) for each
# age class
nbinom_dur2 <- list(ad = fitdistrplus::fitdist(trip_dur$dur[trip_dur$age_fct == "ad"], distr = "nbinom")$estimate,
                    juv = fitdistrplus::fitdist(trip_dur$dur[trip_dur$age_fct == "juv"], distr = "nbinom")$estimate)


# For min 5 km
trip_dur <- vults %>%
  dplyr::select(bird_id, data5) %>%
  unnest(cols = c(bird_id, data5)) %>%
  mutate(trip = paste(bird_id, trip, sep = "_")) %>%
  dplyr::group_by(age_fct, trip) %>%
  dplyr::summarize(dur = max(days_away))

# Find a parametric distribution for trip duration (neg binomial) for each
# age class
nbinom_dur5 <- list(ad = fitdistrplus::fitdist(trip_dur$dur[trip_dur$age_fct == "ad"], distr = "nbinom")$estimate,
                    juv = fitdistrplus::fitdist(trip_dur$dur[trip_dur$age_fct == "juv"], distr = "nbinom")$estimate)

# For min 10 km
trip_dur <- vults %>%
  dplyr::select(bird_id, data10) %>%
  unnest(cols = c(bird_id, data10)) %>%
  mutate(trip = paste(bird_id, trip, sep = "_")) %>%
  dplyr::group_by(age_fct, trip) %>%
  dplyr::summarize(dur = max(days_away))

# Find a parametric distribution for trip duration (neg binomial) for each
# age class
nbinom_dur10 <- list(ad = fitdistrplus::fitdist(trip_dur$dur[trip_dur$age_fct == "ad"], distr = "nbinom")$estimate,
                     juv = fitdistrplus::fitdist(trip_dur$dur[trip_dur$age_fct == "juv"], distr = "nbinom")$estimate)

# vults2 %>%
#    group_by(age_fct, trip) %>%
#    summarize(days_away = max(days_away)) %>%
#    filter(days_away < 25) %>%
#    ggplot() +
#    geom_histogram(aes(x = days_away)) +
#    facet_wrap("age_fct")


# Save as data
nbinom_dur <- nbinom_dur5
usethis::use_data(nbinom_dur, overwrite = TRUE, version = 3)
usethis::use_data(nbinom_dur2, overwrite = TRUE, version = 3)
usethis::use_data(nbinom_dur10, overwrite = TRUE, version = 3)
