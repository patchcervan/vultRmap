# This script keeps record of updates to the colony data

# BEFORE USING THE FUNCTIONS IN THIS SCRIPT IS STRONGLY RECOMMENDED TO
# MAKE A BACK UP COPY OF THE COLONY DATA.

# updateColonyCount() is the workhorse function to update colony data.
# See help("updateColonyCount") for details

library(vultRmap)
library(dplyr)

rm(list = ls())

col_old <- read.csv("../vultRmap_data_aux/colony_data.csv")
# col_other <- read.csv("../vultRmap_data_aux/colony_data (copy).csv")
#
# col_old$id[!col_old$id %in% col_other$id]
# col_other$id[!col_other$id %in% col_old$id]

# Updates 15-07-2021 ------------------------------------------------------

# Colonies "da_227", "da_228", "da_223", "da_225" in Botswana need to have NA counts
colonies <- c("da_227", "da_228", "da_223", "da_225", "da_194")

for(i in seq_along(colonies)){

  updateColonyCount(id_colony = colonies[i],
                    ad_new = NA, ncounts = NA, prop_juv = 0.43,
                    remove_col = FALSE, add_col = FALSE,
                    dir_colony = "../vultRmap_data_aux/colony_data.csv",
                    overwrite = TRUE)

}

# Colonies "da_224", "cvcol718" need to be removed
colonies <- c("da_224", "cvcol229", "cvcol718")

for(i in seq_along(colonies)){

  updateColonyCount(id_colony = colonies[i],
                    ad_new = NA, ncounts = NA, prop_juv = 0.43,
                    remove_col = TRUE, add_col = FALSE,
                    dir_colony = "../vultRmap_data_aux/colony_data.csv",
                    overwrite = TRUE)

}


# Updates 03-08-2021 ------------------------------------------------------

# Add counts to Eastern Cape roosts that appear in Boshoff et al. 2009
col_old %>%
  slice(grep("Martha", col_old$name))

col_old %>%
  slice(grep("Agieskloof", col_old$name))

# Remove duplicated roost
updateColonyCount(id_colony = "cvcol719",
                  ad_new = NA, prop_juv = 0.43,
                  remove_col = TRUE, add_col = FALSE,
                  dir_colony = "../vultRmap_data_aux/colony_data.csv",
                  overwrite = TRUE)

# Add 50 birds to Martha & Mary roost
updateColonyCount(id_colony = "cvcol190",
                  ad_new = 36, ncounts_new = 1, prop_juv = 0.43,
                  remove_col = FALSE, add_col = FALSE,
                  dir_colony = "../vultRmap_data_aux/colony_data.csv",
                  overwrite = TRUE)

# Add 120 birds to Agieskloof roost
updateColonyCount(id_colony = "cvcol115",
                  ad_new = 84, ncounts_new = 1, prop_juv = 0.43,
                  remove_col = FALSE, add_col = FALSE,
                  dir_colony = "../vultRmap_data_aux/colony_data.csv",
                  overwrite = TRUE)


# Updates 04-08-2021 ------------------------------------------------------

# Add last counts for Colleywobbles
col_old %>%
  slice(grep("Colleywobbles", col_old$name))

# The last count was 89 breeding pairs (Vulpro 2020?)
# Considering previous counts the new average is
round((388 * 5 + 89*2) / 6)

updateColonyCount(id_colony = "cvcol379",
                  ad_new = 353, ncounts_new = 6, prop_juv = 0.43,
                  remove_col = FALSE, add_col = FALSE,
                  dir_colony = "../vultRmap_data_aux/colony_data.csv",
                  overwrite = TRUE)

# Update sim files CAUTION! -----------------------------------------------

# THIS WILL DELETE THE SIMULATION FILES FROM COLONIES THAT NO LONGER IN THE
# DATABASE PROCEED WITH CAUTION

# List current colonies
col_data <- read.csv("../vultRmap_data_aux/colony_data.csv")

keep <- col_data %>%
  dplyr::filter(!is.na(avg_ad)) %>%
  dplyr::filter((type == "breed" & avg_ad >= 1) |
                  (type == "roost" & (avg_ad + avg_juv) >= 50)) %>%
  dplyr::pull(id)

# List current simulation files
sim_files <- list.files("../vultRmap_data_aux/col_sims")

# Find files that don't have matching ids in the colony data
sim_ids <- gsub("_ad_sims.rds", "", sim_files)
sim_ids <- gsub("_juv_sims.rds", "", sim_ids)
sim_ids <- gsub("_ad_sims_v2.rds", "", sim_ids)
sim_ids <- gsub("_juv_sims_v2.rds", "", sim_ids)

# MAKE SURE THIS IS WHAT YOU WANT TO REMOVE!
sim_files[!sim_ids %in% keep]

file.remove(paste0("../vultRmap_data_aux/col_sims/", sim_files[!sim_ids %in% keep]))


# If you are happy with the above go ahead and delete files from the other folders

# Define path to files
dirtofiles <- c("../vultRmap_data_aux/col_gam/",
                "../vultRmap_data_aux/col_hgt_sims/",
                "../vultRmap_data_aux/col_hgt_gam/")

# Define suffixes in file names
filesuffix <- c("gam.rds",
                "hgt_sims.rds",
                "hgt_gam.rds")

for(i in seq_along(dirtofiles)){

  # List current simulation files
  sim_files <- list.files(dirtofiles[i])

  # Find files that don't have matching ids in the colony data
  sim_ids <- gsub("_ad_sims.rds", "", sim_files)
  sim_ids <- gsub("_juv_sims.rds", "", sim_ids)
  sim_ids <- gsub("_ad_sims_v2.rds", "", sim_ids)
  sim_ids <- gsub("_juv_sims_v2.rds", "", sim_ids)

  # MAKE SURE THIS IS WHAT YOU WANT TO REMOVE!
  sim_files[!sim_ids %in% keep]

  file.remove(paste0(dirtofiles[i], sim_files[!sim_ids %in% keep]))
}
