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


# Data general covariates -------------------------------------------------------------

# Prepare and save covariates. This takes a while. Don't run if not necessary
# source("data_prep/range_covts_prep.R")

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

# Install -----------------------------------------------------------------

devtools::install()


