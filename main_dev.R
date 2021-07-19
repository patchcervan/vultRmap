# 23-5-2021

# Script to keep track of package development

# DO NOT RUN EVERYTHING AGAIN

# If you want to add a function copy and paste a function block
# at the end of the script (before the install part) and run
# that section that you just pasted.

rm(list = ls())

library(devtools)
library(testthat)


# Create a package structure
# create_package("D:/Documentos/mis_trabajos/Academic/vultRmap")

# Add license
# use_mit_license("Francisco Cervantes Peralta")

# Remember to edit the DESCRIPTION file


# Imports -----------------------------------------------------------------

# Import pipe
use_pipe()

# Prepare Rcpp
use_rcpp()

# Import packages
use_package("dplyr")
use_package("sp")
use_package("rgdal")
use_package("stats")
use_package("furrr")
use_package("future")
use_package("raster")
use_package("mgcv")


# Data general covariates -------------------------------------------------------------

# Prepare and save covariates. This takes a while. Don't run if not necessary
# source("data_prep/range_covts_prep.R")

# Create an ROxygen2 file and document
document()


# Data movement kernel -------------------------------------------------------------

# Load movement kernel from the main project and save as data
# source("data_prep/mov_kernel_prep.R")

# Create an ROxygen2 file and document
document()


# Data SSF fit summary -------------------------------------------------------------

# Load SSF fit summary from the main project and save as data
# source("data_prep/ssf_summary_prep.R")

# Create an ROxygen2 file and document
document()

# Data height model fit summary -------------------------------------------------------------

# Load height fit summary from the main project and save as data
# source("data_prep/hgt_summary_prep.R")

# Create an ROxygen2 file and document
document()

# Rcpp function calcMinDist_cpp --------------------------------------------------

# Add function
use_rcpp("calcMinDist_cpp")

# test locally. Best to Install and Restart
# load_all()

vultRmap::calcMinDist_cpp(r = matrix(1:6, ncol = 2), x = matrix(4:5, ncol = 2))

# Add documentation
# Add ROxygen skeleton manually
document()

check()

# Add tests

use_testthat()

use_test()

test()


# Function calcDist --------------------------------------------------

# Add function
use_r("calcDist")

# test locally
load_all()

calcDist(c(0,0), x = 1:3, y = 1:3)

# Add documentation
# Add ROxygen skeleton manually
document()

check()

# Add tests

use_testthat()

use_test()

test()


# Function calcHabMinDist --------------------------------------------------

# Add function
use_r("calcHabMinDist")

# test locally
load_all()

hab <- expand.grid(-10:10, -10:10)
names(hab) <- c("lon", "lat")
origin <- c(2, 1)
features <- data.frame(lon = c(-5, 5),
                      lat = c(-5, 5))

vultRmap::calcHabMinDist(hab, origin, features)

# Add documentation
# Add ROxygen skeleton manually
document()

check()

# Add tests

use_testthat()

use_test()

test()


# Function prepColHab --------------------------------------------------

# Add function
use_r("prepColHab")

# test locally
load_all()

# Load colony data and sup. feeding sites
col_all <- utils::read.csv("data_aux/colony_data.csv")
sfs <- utils::read.csv("data_aux/sup_feeding_data.csv")
col_all <- read.csv("data_aux/colony_data.csv")
col_cc <- unlist(col_all[7, c("lon", "lat")])
vultRmap::prepColHab(col_cc, 100, col_all, sfs)

# Add documentation
# Add ROxygen skeleton manually
document()

check()

# Add tests

use_testthat()

use_test()

test()


# Function sampleSsfCoef --------------------------------------------------

# Add function
use_r("sampleSsfCoef")

# test locally
load_all()

vultRmap::sampleSsfCoef(nind = 2)

# Add documentation
# Add ROxygen skeleton manually
document()

check()

# Add tests

use_testthat()

use_test()

test()


# Data nbinom_dur -------------------------------------------------------------

# Load tracking data, calculate trip duration, find their distribution and save as data
# source("data_prep/rbinom_dur_prep.R")

# Create an ROxygen2 file and document
document()


# Function completeDataFrame --------------------------------------------------

# Add function
use_r("completeDataFrame")

# test locally
load_all()

vultRmap::completeDataFrame(range_covts, c("disturbed", "dist_col:ttnoon"), 0)

# Add documentation
# Add ROxygen skeleton manually
document()

check()

# Add tests

use_testthat()

use_test()

test()


# Function simTrips --------------------------------------------------

# Add function
use_r("simTrips")

# test locally
load_all()

vultRmap::completeDataFrame(range_covts, c("disturbed", "dist_col:ttnoon"), 0)

# Add documentation
# Add ROxygen skeleton manually
document()

check()

# Add tests

use_testthat()

use_test()

test()


# Function simOneColony --------------------------------------------------

# Add function
use_r("simOneColony")

# test locally
load_all()

# Load colony data to find a colony
col_all <- read.csv("../vultRmap_data_aux/colony_data.csv")

vultRmap::simOneColony(age = "juv",
                       totalsteps = 1000,
                       ncores = 1,
                       col_sel = unlist(col_all[303, c("lon", "lat")]),
                       set_seed = round(runif(1, 1, 1e5)),
                       dist_lim = 500,
                       sample_coefs = NULL)

# Add documentation
# Add ROxygen skeleton manually
document()

check()

# Add tests

use_testthat()

use_test()

test()


# Function simAllColonies --------------------------------------------------

# Add function
use_r("simAllColonies")

# test locally
load_all()

vultRmap::simAllColonies(age = "juv",
                       totalsteps = 1000,
                       ncores = 1,
                       set_seed = round(runif(1, 1, 1e5)),
                       dist_lim = 500,
                       sample_coefs = NULL)

# Add documentation
# Add ROxygen skeleton manually
document()

check()

# Add tests

use_testthat()

use_test()

test()


# Data map_codes -------------------------------------------------------------

# Map codes are obtained from the maps used for fitting the SSF model
# source("data_prep/map_codes_prep.R")

# Create an ROxygen2 file and document
document()


# Function calcUdColony --------------------------------------------------

# Add function
use_r("calcUdColony")

# test locally
load_all()

vultRmap::simAllColonies(age = "juv",
                         totalsteps = 1000,
                         ncores = 1,
                         set_seed = round(runif(1, 1, 1e5)),
                         dist_lim = 500,
                         sample_coefs = NULL)

# Add documentation
# Add ROxygen skeleton manually
document()

check()

# Add tests

use_testthat()

use_test()

test()


# Function calcUdquantile --------------------------------------------------

# Add function
use_r("calcUdquantile")

# test locally
load_all()

# Add documentation
# Add ROxygen skeleton manually
document()

check()

# Add tests

use_testthat()

use_test()

test()

# Function estHgtRisk --------------------------------------------------

# Add function
use_r("estHgtRisk")

# test locally
load_all()

# Add documentation
# Add ROxygen skeleton manually
document()

check()

# Add tests

use_testthat()

use_test()

test()


# Function updateColonyCount --------------------------------------------------

# Add function
use_r("updateColonyCount")

# test locally
load_all()

# Add documentation
# Add ROxygen skeleton manually
document()

check()

# Add tests

use_testthat()

use_test()

test()


# Function calcAng --------------------------------------------------

# Add function
use_r("calcAng")

# test locally
load_all()

# Add documentation
# Add ROxygen skeleton manually
document()

check()

# Add tests

use_testthat()

use_test()

test()

# Install -----------------------------------------------------------------
remove.packages("vultRmap")
devtools::install(type = "source")


