# This script simulates activity around vulture colonies and roosts.

# IT IS CONFIGURED TO USE MANY CORES AND IT IS ADVISED TO RUN IT IN A
# HIGH PERFORMANCE CLUSTER

library(vultRmap)

# Set future maxsize to 650MB
options(future.globals.maxSize = 850*1024^2)

sim_ssf(colonies = c("cvc009", "cvcol115"),
        out_dir = "/home/crvfra001/vults/output/vultRmap/sims",
        data_dir = "/home/crvfra001/vults/data/vultRmap_data_aux",
        seeds = c(6548, 89745),
        totalsteps = 1000,
        ncores = 4,
        dist_lim = 500,
        sample_coefs = 4)
