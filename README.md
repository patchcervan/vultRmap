
<!-- README.md is generated from README.Rmd. Please edit that file -->

# vultRmap: A package to produce a utilization distribution for the population of Cape Vultures

<!-- badges: start -->
<!-- badges: end -->

This package was developed to provide the necessary functionality to
simulate and map Cape Vulture activity around their known colonies and
roosts, for the paper “A utilization distribution for the global
population of Cape Vultures *Gyps coprotheres* to guide wind energy
development”, *Ecological Applications* (2023). Please, refer to this
article for details on the modelling process and project background.

There is a second repository were you can find all the scripts used for
processing the data and run the models that are used for building the
utilization distribution for the population of Cape Vultures. This
package is used, once models are fitted, to produce a map by simulating
vulture activity with the models outputs. The simulation routines were
packaged and separated from the data processing and fitting routines to
be able to easily update the maps, when more/better information becomes
available. Throughout the publication process, we used different
terminology to refer to the same things.

We had three main objectives in mind when developing this package:

1.  Simulate Cape Vulture activity around known colonies and roosts to
    map a spatially-explicit distribution of activity throughout the
    distribution range of the species.

2.  Provide a relatively user-friendly and reproducible way of updating
    the map when more/better data becomes available.

3.  Provide functionality for exploring this map and certain assumptions
    made during the modelling and simulation of Cape Vulture activity.

**Very important:** to be able to replicate or update the maps produced
for the paper, we need data found in another directory called
`vultRmap_data_aux`. This directory is not included in vultRmap because
it contains sensitive tracking, colony, and supplementary feeding site
data, and it will only be made available via motivated request to
Associate Professor Arjun Amar (<arjun.amar@uct.ac.za>). We totally
support open research, but unfortunately different forms of persecution
are still a sad reality for this Vulnerable species.

With the package, though, we provide some testing data, with simulated
colony and supplementary feeding sites locations. These can be used to
test the functionality of the package.

Finally, note that we started thinking of utilizaton distribution as
being indicative of ‘risk’ and therefore you might find references to
risk in some of the functions. In most cases, when we say ‘risk’ we mean
“utilization distribution”.

## Repository structure

This repository has two main components: 1) the `vultRmap` package, and
2) a number of analysis scripts with code to produce and updated
utilization distribution maps.

The analysis scripts can be found in the `analysis` directory and the
`vultRmap` package provides the functionality needed to run these
scripts. The package can be installed by

``` r
remotes::install_github("patchcervan/vultRmap")
```

## Running analysis scripts

The utilization distribution of Cape Vultures are produced by simulating
based on habitat preferences and movement constraints identified by a
step-selection model. The simulation routines can be found in the
`analysis/update_ALL` directory.

Simulations are computationally very intensive and it is recommended to
run them in a high performance cluster. We used a 20 core machine and
200GB of RAM to run simulations for all colonies and it took a couple of
days to run.

Also, remember that to be able to run these routines, we need the data
contained in `vultRmap_data_aux`.

Once all the pre-requirements are met, we can start producing maps.
Paths to specific directories need to be adapted to the user
configuration. This is true also for the code contained in the scripts.

### Predicting utilization distributions

``` r

# Run simulations around all identified colonies
source("analysis/update_ALL/1_ssf_sims_all.R")

# Predict flight height for simulated Cape Vulture locations
source("analysis/update_ALL/1_hgt_risk_all.R")

# Smooth utilization distribution using GAMs
source("analysis/update_ALL/3_ssf_gam_all.R")

# Smooth utilization distribution at collision risk height GAMs
source("analysis/update_ALL/4_hgt_gam_all.R")
```

### Producing utilization distribution maps

``` r

# Compute cumulative UD and produce raster maps
source("analysis/update_ALL/5_make_all_risk_maps.R")

# Save maps in HTML formats
source("analysis/update_ALL/6_make_all_html_maps.R")
```

### Updating utilization distribution maps

If new colony and/or roost data is collected, the first thing we need to
do, before updating the utilization distribution maps, is to update our
colony database.

At this point is really IMPORTANT TO MAKE A COPY OF THE COLONY DATABASE
BEFORE MAKING ANY CHANGES. The following procedures will modify the
database without the possibility of going back to what it was.

The function `updateColonyCount()` is the workhorse function to update
colony data. See `help(updateColonyCount)` for details.

The script `vultRmap/analysis/0_colony_updates/` is used to keep track
of the updates made to the database so far. In this file there are some
examples of updates that had be done before the paper was published.

Once the updates to the colony database are completed, we must proceed
to produce new Cape Vulture activity simulations for those colonies that
we need updates for.

The procedure for simulating around new colonies would be the same used
in the previous section. However, in this case we will have to specify
the colonies we want to simulate activity for. For this we go to
`vultRmap/analysis/update_ONE` and run the two scripts in this
directory, modifying the functions `1_ssf_sim` and `2_hgt_sim` to
specify the colony codes we want simulations for. For example, the
following code will simulate activity around colonies “cvcol009” and
“cvcol115”.

``` r
sim_ssf(colonies = c("cvc009", "cvcol115"),
        out_dir = "/home/vultRmap/sims",
        data_dir = "/home/vultRmap_data_aux",
        seeds = c(6548, 89745),
        totalsteps = 1000,
        ncores = 4,
        dist_lim = 500,
        sample_coefs = 4)
```

Similarly, this code will simulate flight height for the activity
simulations

``` r
sim_height_risk(colonies = c("cvc009", "cvcol115"),
                data_dir = "../vultRmap_data_aux/",
                sims_dir = "../vultRmap_data_aux/col_sims/",
                out_dir = "../vultRmap_data_aux/col_hgt_sims/",
                ncoefs = 40,
                seed = 87634)
```

As previously mentioned, activity simulations are computationally
demanding, so both scripts use the capabilities of the `furrr` package
to run the simulations in parallel. It is recommended to use a machine
with at least 4 cores and 16 GB of RAM for running simulations for
individual colonies.

After simulating activity for the new colonies, we need to smooth the
UDs for all colonies combined, just as we did earlier.

``` r

# Smooth utilization distribution using GAMs
source("analysis/update_ALL/3_ssf_gam_all.R")

# Smooth utilization distribution at collision risk height GAMs
source("analysis/update_ALL/4_hgt_gam_all.R")
```

And produce the utilization distribution maps with the new information.

``` r

# Compute cumulative UD and produce raster maps
source("analysis/update_ALL/5_make_all_risk_maps.R")

# Save maps in HTML formats
source("analysis/update_ALL/6_make_all_html_maps.R")
```

### Fun with vultRsims

Finally, the Shiny App `vultRsims` can be used to explore the workings
of these simulation routines in a relatively user-friendly way. The app
is in the `analysis` directory and with it you can simulate vulture
activity using simulated colony and supplementary feeding site data. You
can also change some of the simulation parameters to investigate their
effects in the results. The only difference is that the final
utilization distribution is calculated using the kernel density estimate
of the simulated points rather than counting visits to square grid
cells, smoothing, etc.
