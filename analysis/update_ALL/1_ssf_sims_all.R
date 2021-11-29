# This script simulates activity around vulture colonies and roosts.

# IT IS CONFIGURED TO USE MANY CORES AND IT IS ADVISED TO RUN IT IN A
# HIGH PERFORMANCE CLUSTER

library(vultRmap)

# Set future maxsize to 650MB
options(future.globals.maxSize = 850*1024^2)

# Define minimum size of colony (number of adults) and
# roosts (total number of birds)
min_size_col <- 1
min_size_roost <- 50

# We will need to calculate distance to other colonies
col_all <- utils::read.csv("/home/crvfra001/vults/data/vultRmap_data_aux/colony_data.csv")

# For debugging
# col_all <- utils::read.csv("../vultRmap_data_aux/colony_data.csv")

# Subset colonies to we have counts for
col_to_pred <- col_all %>%
      dplyr::filter(!is.na(avg_ad)) %>%
      dplyr::filter((type == "breed" & avg_ad >= min_size_col) |
                          (type == "roost" & (avg_ad + avg_juv) >= min_size_roost))

# In case we want to exclude some colonies
# col_to_pred <- col_to_pred[-(1:3),]

# Define ages
ages <- c("ad", "juv")

# And output suffixes
suff <- c("", "_v2")

for(k in 1:2){
  for(j in 1:2){

    vultRmap::simAllColonies(col_to_pred = col_to_pred,
                             age = ages[j],
                             totalsteps = 120000,
                             ncores = 40,
                             set_seed = 6548,
                             dist_lim = 1200,
                             sample_coefs = 40,
                             out_dir = "/home/crvfra001/vults/output/vultRmap/sims",
                             data_dir = "/home/crvfra001/vults/data/vultRmap_data_aux",
                             out_suffix = suff[k])

  }
}
