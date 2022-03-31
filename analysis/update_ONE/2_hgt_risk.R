
# In this script we estimate the probability of flying at height risk for each
# of the simulated activity points.

library(vultRmap)

rm(list = ls())


# Predict for all colonies ------------------------------------------------

future::plan("multisession", workers = 8)

sim_height_risk(colonies = c("cvc009", "cvcol115"),
                data_dir = "../vultRmap_data_aux/",
                sims_dir = "../vultRmap_data_aux/col_sims/",
                out_dir = "../vultRmap_data_aux/col_hgt_sims/",
                ncoefs = 40,
                seed = 87634)

future::plan("sequential")
