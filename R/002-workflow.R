#########################
#########################
#### workflow.R

#### Aims:
# (1) Simulate and analyse animal tracking data using patter

#### Prerequisites
# (1) Prepare map (see prepare-map.R)


#########################
#########################
#### Set up

#### Wipe workspace
rm(list = ls())
try(pacman::p_unload("all"), silent = TRUE)
dv::clear()

#### Essential packages
library(dv)
library(collapse)
library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)
library(glue)
library(JuliaCall)
library(patter)
library(prettyGraphics)
library(spatstat.explore)
library(testthat)
library(tictoc)
julia_save <- patter:::julia_save
julia_load <- patter:::julia_load
julia_code <- patter:::julia_code
dv::src()

#### Load data
# (map   <- dat_gebco())
map    <- terra::rast(here_data("spatial", "map.tif"))
map_ud <- terra::rast(here_data("spatial", "map-ud.tif"))
mpa    <- terra::vect(readRDS(here_data("spatial", "mpa.rds")))
coast  <- readRDS(here_data("spatial", "coast.rds"))
# win    <- qs::qread(here_data("spatial", "win.qs"))

#### Global settings & parameters
spatstat.options(npixel = 500)
op         <- options(terra.pal = rev(terrain.colors(256)))
overwrite  <- FALSE
tic()


#########################
#########################
#### Initiation

#### Connect to Julia & set seed (~20 s)
julia_connect(.threads = 10L)
set_seed(20240601L)

#### Define map
# Read the env = map.JLD2 directly for speed
patter:::julia_load(here_data("spatial", "map.JLD2"), "env")

#### Define study period
timeline <- seq(as.POSIXct("2023-01-01 12:00:00", tz = "UTC"),
                as.POSIXct("2023-01-31 23:58:00", tz = "UTC"),
                by = "2 mins")
# timeline <- timeline[1:500]


#########################
#########################
#### Simulation


#########################
#### Simulate an acoustic array

moorings <-
  run(here_data("sim", "moorings.rds"),
      overwrite = overwrite,
      expr = {
        moorings <- sim_array(.map = map,
                              .timeline = timeline,
                              .n_receiver = 100L,
                              .arrangement = "regular")
      }
  )


#########################
#### Simulate a movement path

#### Define a tagging location (origin)
# Define origin
x <- 708903.5
y <- 6252070
xinit <- data.table(map_value = terra::extract(map, cbind(x, y))[1, 1],
                    x = x, y = y)
saveRDS(xinit, here_data("sim", "xinit.rds"))
# Validate origin
expect_true(!is.na(xinit$map_value))
# Map
terra::plot(map)
terra::lines(mpa)
points(xinit$x, xinit$y, col = "red")

#### Define the state type & the movement model
state      <- "StateXY"
model_move <- move_xy()

#### Simulate movement (<1 s)
paths <-
  run(here_data("sim", "paths.rds"),
      overwrite = overwrite,
      expr = {
        paths <- sim_path_walk(.map = map,
                               .timeline = timeline,
                               .state = state,
                               .model_move = model_move,
                               .xinit = xinit)
        julia_save("paths", here_data("sim", "paths.jld2"))
        paths
      }
  )
expect_true(all(!is.na(paths$map_value)))
julia_load(here_data("sim", "paths.jld2"))

#### (optional) UD for simulated path (~1 s)
path_ud <- map_dens(.map = map,
                    .coord = readRDS(here_data("sim", "paths.rds")),
                    sigma = bw.diggle)$ud
terra::writeRaster(path_ud,
                   here_data("ud", "ud-path-bw.diggle.tif"),
                   overwrite = TRUE)

#### Define recapture location
xinit_end <- paths[.N, .(map_value, x, y)]

#### Visualise depth trajectory
plot(paths$timestamp, paths$map_value * -1, type = "l")
abline(h = 0, col = "royalblue")


#########################
#### Simulate observations

#### Define observation models
model_obs <- c("ModelObsAcousticLogisTrunc", "ModelObsDepthUniform")

#### Define acoustic model parameters
pars_acc <-
  moorings |>
  select(sensor_id = "receiver_id",
         "receiver_x", "receiver_y",
         "receiver_alpha", "receiver_beta", "receiver_gamma") |>
  as.data.table()

#### Define archival parameters ('perfect')
pars_arc_perfect <- data.table(sensor_id = 1L,
                               depth_shallow_eps = 0,
                               depth_deep_eps = 0)

#### Define archival parameters ('imperfect')
# (Check 'realistic' parameter values for the study area)
pars_arc_imperfect <- data.table(sensor_id = 1L,
                                 depth_shallow_eps = 20,
                                 depth_deep_eps = 20)

##### Simulate observations (perfect) (~10 s)
obs_perfect <-
  run(file = here_data("sim", "obs-perfect.rds"),
      overwrite = overwrite,
      expr = {
        obs <-
          sim_observations(.timeline = timeline,
                           .model_obs = model_obs,
                           .model_obs_pars = list(pars_acc, pars_arc_perfect))
        julia_save("yobs", here_data("sim", "obs-perfect.jld2"))
        obs
      }
  )
# julia_load(here_data("sim", "obs-perfect.jld2"), "yobs")

#### Simulate observations (imperfect) (~10 s)
obs_imperfect <-
  run(file = here_data("sim", "obs-imperfect.rds"),
      overwrite = overwrite,
      expr = {
        obs <-
          sim_observations(.timeline = timeline,
                           .model_obs = model_obs,
                           .model_obs_pars = list(pars_acc, pars_arc_imperfect))
        julia_save("yobs", here_data("sim", "obs-imperfect.jld2"))
        obs
      }
  )
# julia_load(here_data("sim", "obs-imperfect.jld2"), "yobs")

#### Collate detections
detections_perfect <-
  obs_perfect$ModelObsAcousticLogisTrunc[[1]] |>
  filter(obs == 1L) |>
  mutate(timestep = paths$timestep[match(timestamp, paths$timestamp)]) |>
  as.data.table()
detections_imperfect <-
  obs_imperfect$ModelObsAcousticLogisTrunc[[1]] |>
  filter(obs == 1L) |>
  mutate(timestep = paths$timestep[match(timestamp, paths$timestamp)]) |>
  as.data.table()
saveRDS(detections_perfect, here_data("sim", "detections-perfect.rds"))
saveRDS(detections_imperfect, here_data("sim", "detections-imperfect.rds"))

#### Collate archival (depth) time series
archival_perfect   <- data.table(timestep = paths$timestep,
                                 depth = obs_perfect$ModelObsDepthUniform[[1]]$obs * -1)
archival_imperfect <- data.table(timestep = paths$timestep,
                                 depth = obs_imperfect$ModelObsDepthUniform[[1]]$obs * -1)
saveRDS(archival_perfect, here_data("sim", "archival-perfect.rds"))
saveRDS(archival_imperfect, here_data("sim", "archival-imperfect.rds"))


#########################
#########################
#### Particle filter

#### Define common filter arguments
# Resample at every time step for maxlp figure
pargs <- list(.map = map,
             .timeline = timeline,
             .model_obs = model_obs,
             .model_move = model_move,
             .n_particle = 1e5L,
             .n_resample = 1e5L)

##### Forward run (perfect): 297.059 s (4.95 mins)
# Record one copy of the correct location @ each time step
pargs$.n_record   <- 1L
pargs$.yobs       <- list(obs_perfect$ModelObsAcousticLogisTrunc[[1]],
                          obs_perfect$ModelObsDepthUniform[[1]])
pargs$.direction  <- "forward"
pargs$.xinit      <- xinit
pff_perfect      <-
  run(file = here_data("filter", "pff-perfect.rds"),
      overwrite = overwrite,
      expr = {
        out <- do.call(pf_filter, pargs, quote = TRUE)
        julia_save("pfwd", here_data("filter", "pff-perfect.jld2"))
        out
      })
expect_true(pff_perfect$convergence)
expect_false(any(is.na(pff_perfect$states)))
# julia_load(here_data("pff-perfect.jld2"), "pfwd")

#### Forward run (imperfect): 371.915 s (6.20 mins)
pargs$.n_record <- 1000L
pargs$.yobs     <- list(obs_imperfect$ModelObsAcousticLogisTrunc[[1]],
                         obs_imperfect$ModelObsDepthUniform[[1]])
pff_imperfect   <-
  run(file = here_data("filter", "pff-imperfect.rds"),
      overwrite = overwrite,
      expr = {
        out <- do.call(pf_filter, pargs, quote = TRUE)
        julia_save("pfwd", here_data("filter", "pff-imperfect.jld2"))
        out
      })
expect_true(pff_imperfect$convergence)
expect_false(any(is.na(pff_imperfect$states)))
julia_load(here_data("filter", "pff-imperfect.jld2"), "pfwd")

#### Backward run (imperfect): ~370.854 s (6.19 mins)
pargs$.direction <- "backward"
pargs$.xinit     <- xinit_end
pfb_imperfect    <-
  run(file = here_data("filter", "pfb-imperfect.rds"),
      overwrite = overwrite,
      expr = {
        out <- do.call(pf_filter, pargs, quote = TRUE)
        julia_save("pbwd", here_data("filter", "pfb-imperfect.jld2"))
        out
      })
expect_true(pfb_imperfect$convergence)
expect_false(any(is.na(pfb_imperfect$states)))
julia_load(here_data("filter", "pfb-imperfect.jld2"), "pbwd")

#### Basic checks
# Confirm that Julia and R outputs for the backward filter match at a selected time step
julia_code(
  '
  s = pbwd.state[:, 2]
  n = length(s)
  x = [s[i].x for i in 1:n]
  y = [s[i].y for i in 1:n]
  '
)
x0 <- pfb_imperfect$states[timestep == 2, x]
y0 <- pfb_imperfect$states[timestep == 2, y]
x1 <- julia_eval("x")
y1 <- julia_eval("y")
expect_equal(x0, x1)
expect_equal(y0, y1)
pp <- par(mfrow = c(1, 2))
terra::plot(map)
points(x0, y0)
terra::plot(map)
points(x1, y1)
par(pp)


#########################
#########################
#### Two filter smoother

#### Implement the smoother 1207.22 s (20.12 mins)
# (Use pfwd and pbwd from the imperfect run)
smo <- run(file = here_data("smoother", "tff.rds"),
           overwrite = overwrite,
           expr = {
             smo <- pf_smoother_two_filter()
             julia_save("ptf", here_data("smoother", "ptf.jld2"))
             smo
           })
expect_false(any(is.na(smo$states)))
julia_load(here_data("smoother", "ptf.jld2"), "ptf")


#########################
#########################
#### Analysis

#########################
#### 'Perfect' run

#### Validate states
# Extract states
s <- pff_perfect$states
# Confirm simulated and reconstructed locations match (at grid cell level)
s[, cell_id := terra::cellFromXY(map, cbind(x, y))]
paths[, cell_id := terra::cellFromXY(map, cbind(x, y))]
expect_equal(s$cell_id, paths$cell_id)
# Confirm map_value (depths) match
expect_equal(paths$map_value, s$map_value)

#### Visualise reconstructed path
terra::plot(map, col = "lightgrey")
patter:::add_sp_path(x = s$x, y = s$y, length = 0.01)


#########################
#### 'Imperfect' run

#### Filter diagnostics
# diag <- pff_imperfect$diagnostics
diag <- pfb_imperfect$diagnostics
# (optional) Subsampling
# > This is required for smoother plots
diag <-
  diag |>
  lazy_dt() |>
  mutate(timestep = plyr::round_any(timestep, accuracy = 100, f = floor)) |>
  group_by(timestep) |>
  summarise(ess = min(ess),
            maxlp = min(maxlp)) |>
  as.data.table()

#### Plot diagnostics
png(here_fig("diagnostics.png"),
    height = 5, width = 7, units = "in", res = 600)
pp <- set_par()

## ESS
# 500 particles should be sufficient to approximate a 2D distribution
# Here, we have some time steps with fewer particles
# But it is also the case at at some points there are few locations
# ... for the animal (e.g., in a deep hole)
xlim <- c(0, max(diag$timestep))
min(diag$ess)
axis_ls <- pretty_plot(diag$timestep, diag$ess,
                       pretty_axis_args = list(pretty = list(n = 3), cex.axis = 0.7),
                       xlim = xlim,
                       xlab = "", ylab = "",
                       type = "l")
lines(range(diag$timestep), c(500, 500), col = "red", lty = 3)
# add_detections(detections_imperfect$timestep, axis_ls[[2]]$lim[2])

## maxlp
# Here, we are looking for any EXTREME low values
# Note that in reality there may be time steps where the simulated animal behaved in an unlikely way
par(new = TRUE)
exp(min(diag$maxlp))
pretty_plot(diag$timestep, diag$maxlp,
            pretty_axis_args = list(side = c(1, 4),
                                    pretty = list(n = 3),
                                    axis = list(list(NULL), list(col = "blue", col.axis = "blue")),
                                    cex.axis = 0.7),
            xlim = xlim,
            xlab = "", ylab = "",
            col = "blue",
            type = "l")

par(pp)
dev.off()

#### Visualise probability distribution at selected time steps
# Select particles from an example time step, t
t <- 2
s <- pff_imperfect$states
s <- s[s$timestep == t, ]
# Define zoomed map
map_zoom <- terra::crop(map_ud, # map_ud
                        terra::ext(min(s$x), max(s$x), min(s$y), max(s$y)) + 100)
terra::plot(map_zoom)
points(s$x, s$y)
# Create a quick zoomed-in map (~1 s)
# sigma: bw.diggle, bw.CvL, bw.scott, bw.ppl.
# * bw.diggle: fragmented
# * bw.CvL: oversmoothed
# * bw.scott: ok
# * bw.ppl: looks best
ud <- map_dens(map_zoom,
               .coord = s,
               .discretise = FALSE,
               sigma = bw.ppl,
               .plot = FALSE)$ud
terra::plot(ud)
points(s$x, s$y, pch = ".")
points(paths$x[t], paths$y[t], pch = 4, cex = 3, lwd = 3, col = "red")
terra::lines(coast)

#### Estimate UD

## Timings:
# * Building XYM: ~12 mins
# * Building PPP: ~7 mins
# * UD estimation:
# - 28 mins:    bw.diggle, 500 pixels
# - 19 secs:    bw.scott, 500 pixels
# - >24 hours:  bw.ppl, 500 pixels (cancelled)
# - NA          bw.CvL (not implemented, typically massively oversmooths)

## (optional) Define restricted window around particles
# Define window
# * This helps massively with estimation speed
win <- as.owin.fol(.poly = coast,
                   .map = map_ud,
                   .x = smo$states$x, .y = smo$states$y, .buffer = 2000)
# Define blank SpatRaster onto which we can add the UD from the restricted window
blank <- terra::deepcopy(map_ud)
blank[blank > 0] <- 0
# terra::plot(blank)

## Define sigma options
shortcut <- list()
if (file.exists(here_data("ud", "x.rds")) & isFALSE(overwrite)) {
  shortcut <- list(x = readRDS(here_data("ud", "x.rds")))
}
sigmas <- list(bw.diggle = bw.diggle,
               bw.scott = bw.scott,
               bw.ppl = bw.ppl)

## Estimate UDs
tic()
for (i in seq_len(length(sigmas))) {
  outfile  <- here_data("ud", paste0("ud-", names(sigmas)[i], ".tif"))
  if (!file.exists(outfile) | overwrite) {

    # Estimate UD
    # * We have defined the pixel resolution via spatstat.options(npixel)
    shortcut <- map_dens(.map = terra::deepcopy(map_ud),
                         .owin = win,
                         .coord = smo$states, # smo$states[1:10, ],
                         .shortcut = shortcut,
                         sigma = sigmas[[i]])

    # Merge UD onto entire study area
    ud <- terra::merge(shortcut$ud, blank)
    terra::plot(ud)

    # Visual check for specific region
    if (FALSE) {
      sbb <- terra::ext(c(range(smo$states$x[1:10]), range(smo$states$y[1:10]))) + 2500
      x <- terra::crop(map_ud, sbb)
      x <- terra::crop(shortcut$ud, sbb)
      terra::plot(x)
      spatGrid(x, lwd = 0.1)
      spatPoints(smo$states$x[1:10], smo$states$y[1:10], cex = 2)
      spatPoints(shortcut$x$x, shortcut$x$y, col = "red")
    }

    # Save outputs
    if (i == 1L) {
      saveRDS(shortcut$x, here_data("ud", "x.rds"))
    }
    terra::writeRaster(ud, outfile, overwrite = TRUE)

  }
  cat("\n\n")
}
toc()

#### Extract positions in MPA
if (isTRUE(overwrite)) {

  # Define fishing open/closed regions in MPA
  mpa$fishing <- plyr::mapvalues(mpa$open, c(FALSE, TRUE), c("closed", "open"))
  mpa$fishing <- factor(mpa$fishing)
  mpa$open <- mpa$col_index <- mpa$id <- NULL

  # Check the behaviour of terra extract():
  # * This returns a data.frame with a fishing column (closed/open):
  terra::extract(mpa, cbind(paths$x[1:2], paths$y[1:2]))
  # * A point outside the MPA returns N/A for fishing:
  terra::extract(mpa, cbind(680540.6, 6238276))$fishing

  # Estimate the true time spent in the MPA (~17 s)
  tic()
  paths[, mpa := terra::extract(mpa, cbind(x, y))$fishing]
  toc()

  # Estimate the time spend in the MPA from particles (~4.49 hours)
  tic()
  s <- copy(smo$states)
  s[, mpa := terra::extract(mpa, cbind(x, y))$fishing]
  toc()

  # Write data.table(s) to file
  qs::qsave(paths, here_data("data.table", "paths.qs"))
  qs::qsave(s, here_data("data.table", "states.qs"))

} else {

  # Read data.table(s)
  paths <- qs::qread(here_data("data.table", "paths.qs"))
  s     <- qs::qread(here_data("data.table", "states.qs"))

}

#### Compare true & estimated time spent in MPA
# > Number of path coordinates in the MPA / number of path coordinates in total
length(which(paths$mpa %in% c("open", "closed"))) / nrow(paths)
# 0.3627505
length(which(paths$mpa %in% "closed")) / nrow(paths)
# 0.3537796
# > Number of particles in the MPA / total number of particles
length(which(s$mpa %in% c("open", "closed"))) / nrow(s)
# 0.3695122
length(which(s$mpa %in% "closed")) / nrow(s)
# 0.3595391


toc()

#### End of code.
#########################
#########################
