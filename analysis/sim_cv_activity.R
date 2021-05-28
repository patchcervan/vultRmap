library(vultRmap)

rm(list = ls())


# Read in necessary data --------------------------------------------------

# We will need to calculate distance to other colonies
col_all <- read.csv("data_aux/colony_data.csv")

# And to supplementary feeding sites
sfs <- read.csv("data_aux/sup_feeding_data.csv")

# If you are updating the map, consider if these files need to be updated too!


# Prepare habitat for simulations -----------------------------------------

# This may take some minutes if max_range is large (>500)
system.time(
  hab <- vultRmap::prepColHab(col_cc = unlist(col_all[7, c("lon", "lat")]), max_range = 1000,
                              col_all = col_all, sfs = sfs)
)



