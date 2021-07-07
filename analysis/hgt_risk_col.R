library(tidyverse)
library(vultRmap)

rm(list = ls())

# Load colony data
col_all <- read.csv("../vultRmap_data_aux/colony_data.csv")

# Subset those colonies and roosts we have data for
col_to_pred <- col_all %>%
   filter(!is.na(avg_ad)) %>%
   filter((type == "breed" & avg_ad > 0) | (type == "roost" & (avg_ad + avg_juv) > 50))

# Extract model coefficients
hgt_coef <- sampleHgtCoef(hgt_fit_summary, 40, seed = 87634)


# Simulate for one colony -------------------------------------------------

# Select one colony
col_sel <- col_to_pred[col_to_pred$id == "cvcol414",]
hgt_risk <- estHgtRisk(.col_sel = col_sel,
                       .coefs = hgt_coef, .age = "ad",
                       .datadir = "../vultRmap_data_aux/",
                       .simsdir = "../vultRmap_data_aux/col_sims/",
                       .outdir = "../vultRmap_data_aux/col_hgt_risk/")


# Predict for all colonies ------------------------------------------------

future::plan("multisession", workers = 5)

ages <- c("ad", "juv")

for(j in 1:2){

  furrr::future_map(seq_len(nrow(col_to_pred)),
                     ~estHgtRisk(.col_sel = col_to_pred[.x, ],
                                 .coefs = hgt_coef, .age = ages[j],
                                 .datadir = "../vultRmap_data_aux/",
                                 .simsdir = "../vultRmap_data_aux/col_sims/",
                                 .outdir = "../vultRmap_data_aux/col_hgt_risk/"),
                     .options = furrr::furrr_options(seed = TRUE))

}

future::plan("sequential")
