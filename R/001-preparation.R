#########################
#########################
#### preparation.R

#### Aims:
# (1) Prepare simulations:
# * Prepare a study area map

#### Prerequisites
# (1) Copy raw data from patter-flapper


#########################
#########################
#### Set up

#### Wipe workspace
rm(list = ls())
try(pacman::p_unload("all"), silent = TRUE)
dv::clear()

#### Essential packages
library(collapse)
library(dv)
library(patter)
library(testthat)
library(tictoc)

#### Load data
# (map   <- dat_gebco())
map   <- terra::rast(here_data("spatial", "bathy.tif"))
mpa   <- terra::vect(readRDS(here_data("spatial", "mpa.rds")))

#### Global settings & parameters
op <- options(terra.pal = rev(terrain.colors(256)))


#########################
#########################
#### Prepare map

#### Connect to Julia (~20 s)
julia_connect(.threads = "auto")

#### Pull map into memory (~4 s)
tic()
terra:::readAll(map)
toc()

#### Visualise study area
map   <- terra::rast(here_data("spatial", "bathy.tif"))
terra::plot(map)
terra::lines(mpa)

#### Simplify map
# (optional) Crop map to smaller area (for 1 month simulation)
# * Define limits
# * These are based on trial-and-error
# * For illustation purposes, we want a study area
# * ... that is suitably sized relative to the size of simulated movements
xlim <- c(674064.9, 719999.5)
ylim <- c(6225144, 6290398)
map  <- terra::crop(map, terra::ext(c(xlim, ylim)))
# map <- terra::crop(map, terra::ext(658818.1, 719999, 6200702, 6298031)
# (optional) Aggregate the map
# * This improves speed
# * & reduces the number of particles required for convergence of the 'perfect' simulation
( map <- terra::aggregate(map, fact = 20, "mean") )
# Visualise simplified map
terra::plot(map)
terra::lines(mpa)

#### Add random noise (~<20 s)
# NB this solution is imperfect
# There may be cells that are unique but effectively identical due to floating point precision
tic()
n <- terra::ncell(map)
map[] <- map[] + rnorm(n, mean = 5, sd = 1) + runif(n, 0, 20)
expect_true(terra::global(map, "min", na.rm = TRUE) > 0)
toc()
# Check the number of unique values (~20 s)
tic()
map_vals <- na.omit(map[])
fndistinct(map_vals)
length(map_vals)
toc()

#### Set map in Julia (~20 s)
tic()
set_map(map)
toc()

#### Write map(s) to file
terra::writeRaster(map * -1,
                   here_data("spatial", "map-negative.tif"),
                   overwrite = TRUE)
terra::writeRaster(map,
                   here_data("spatial", "map.tif"),
                   overwrite = TRUE)
patter:::julia_save("env", here_data("spatial", "map.JLD2"))


#### End of code.
#########################
#########################
