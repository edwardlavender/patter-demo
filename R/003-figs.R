#########################
#########################
#### figs.R

#### Aims:
# (1) Generate manuscript figures

#### Prerequisites
# (1) Prepare map (see prepare-map.R)
# (2) Run workflow
# > This code used hard coded parameters used in the workflow.
# > If these are updated in workflow.R, this file also requires updating.


#########################
#########################
#### Set up

#### Wipe workspace
rm(list = ls())
try(pacman::p_unload("all"), silent = TRUE)
dv::clear()

#### Essential packages
library(dv)
library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)
library(patter)
library(prettyGraphics)
library(spatstat.explore)
library(truncdist)
library(testthat)
library(tictoc)
dv::src()
add_sp_path <- patter:::add_sp_path

#### Load data
map    <- terra::rast(here_data("spatial", "map-negative.tif"))
map_ud <- terra::rast(here_data("spatial", "map-ud.tif"))
mpa    <- terra::vect(readRDS(here_data("spatial", "mpa.rds")))
coast  <- readRDS(here_data("spatial", "coast.rds"))
paths    <- readRDS(here_data("sim", "paths.rds"))
xinit    <- readRDS(here_data("sim", "xinit.rds"))
moorings <- readRDS(here_data("sim", "moorings.rds"))
detections_perfect   <- readRDS(here_data("sim", "detections-perfect.rds"))
detections_imperfect <- readRDS(here_data("sim", "detections-imperfect.rds"))
archival_perfect     <- readRDS(here_data("sim", "archival-perfect.rds"))
archival_imperfect   <- readRDS(here_data("sim", "archival-imperfect.rds"))
pff_imperfect        <- readRDS(here_data("filter", "pff-imperfect.rds"))
pfb_imperfect        <- readRDS(here_data("filter", "pfb-imperfect.rds"))
tff                  <- readRDS(here_data("smoother", "tff.rds"))
ud                   <- terra::rast(here_data("ud", "ud-bw.diggle.tif"))

#### Global settings & parameters
spatstat.options(npixel = 500)
op <- options(terra.pal = rev(terrain.colors(256)))


#########################
#########################
#### Graphical settings

#### Pretty axes
paa <- list(pretty = list(n = 3))

#### Bathymetry colour scheme
# Bathymetry colour scheme
range(c(unlist(terra::global(map, "range", na.rm = TRUE)), archival_perfect$depth, archival_imperfect$depth))
bathy_zlim <- c(-320, 0)
bathy_scheme <- pretty_cols_brewer(bathy_zlim,
                                   scheme = "Blues",
                                   n_breaks = diff(bathy_zlim),
                                   rev = TRUE)
bathy_cols <- scales::alpha(bathy_scheme$col, 0.75)
# Depth colour scheme
depth_scheme <-
  data.frame(depth = bathy_scheme$breaks[1:(length(bathy_scheme$breaks) - 1)],
             col = bathy_cols)
# Archival colours (perfect)
archival_perfect$breaks <- cut(archival_perfect$depth,
                               depth_scheme$depth,
                               labels = FALSE)
archival_perfect$col <- depth_scheme$col[archival_perfect$breaks]
# Archival colours (imperfect)
archival_imperfect$breaks <- cut(archival_imperfect$depth,
                                 depth_scheme$depth,
                                 labels = FALSE)
archival_imperfect$col <- depth_scheme$col[archival_imperfect$breaks]

#### Moorings/detections (red, green)
# Define colours
moorings[, col := ifelse(moorings$receiver_id %in%
                           c(detections_perfect$perfect,
                             detections_imperfect$sensor_id),
                             yes = "darkgreen",
                             no = "darkred")]
# Define SpatVector
moorings_vect <-
  moorings |>
  select(x = receiver_x, y = receiver_y, col = col) |>
  terra::vect(geom = c("x", "y")) |>
  terra::buffer(width = 750)

#### UD sigma method
pp <- par(mfrow = c(1, 2))
terra::plot(terra::rast(here_data("ud", "ud-bw.diggle.tif")))
terra::plot(terra::rast(here_data("ud", "ud-bw.scott.tif")))
par(pp)


#########################
#########################
#### Fig. 1.

#### Approx. caption
'
Figure 1. An overview of the patter package.
'


#########################
#########################
### Fig. 2.

#### Approx. caption
'
Figure 2. The components of a state-space model for animal tracking data. A shows a hypothetical area of interest, including a simulated movement path and acoustic receivers. B–C show the components of the movement process used to simulate and model movements (i.e., the probability density distributions for step lengths and turning angles). D–E show the observation models used to simulate and model acoustic and archival observations arising from the simulated path. F shows the simulated time series.
'

#### Simulated study area
png(here_fig("study-area.png"),
    height = 5, width = 5, units = "in", res = 600)
# Define region of interest
bb <- terra::ext(map)
xlim <- bb[1:2]
ylim <- bb[3:4]
# Plot map
pp <- set_par()
terra::plot(map,
            col = bathy_cols, smooth = TRUE,
            xlim = xlim, ylim = ylim,
            range = bathy_zlim,
            axes = FALSE)
axis(side = 1, at = xlim, pos = ylim[1], labels = FALSE)
axis(side = 2, at = ylim, pos = xlim[1], labels = FALSE)
axis(side = 3, at = xlim, pos = ylim[2], labels = FALSE)
axis(side = 4, at = ylim, pos = xlim[2], labels = FALSE)
pretty_axis(side = 1:2,
            lim = list(xlim, ylim),
            pretty = list(n = 3),
            control_sci_notation = list(magnitude = Inf, digits = 0L),
            control_axis = list(las = FALSE, cex.axis = 0.9),
            add = TRUE)
par(pp)
# Add simulated path
# * Note the addition of blank spatPoints() is required for correct plotting of path
col_path <- viridis::inferno(nrow(paths), direction = -1)
spatPoints(paths$x, paths$y, col = NA)
add_sp_path(paths$x, paths$y, length = 0.01, lwd = 0.25,
         col = col_path)
# Add coast
terra::lines(coast, col = "dimgrey", lwd = 0.5)
terra::lines(mpa, col = "darkblue", lwd = 2)
# Add receivers & origin
terra::plot(moorings_vect, col = moorings_vect$col, border = moorings_vect$col, add = TRUE)
spatPoints(xinit$x, xinit$y, col = "black", pch = 4, lwd = 3)
# Add scale bars
terra::sbar(d = 5000, xy = cbind(714500, 6225700), halo = FALSE)
terra::north(d = 4000, xy = cbind(676060.1, 6276000), lwd = 2)
dev.off()

# Create a quick legend for the path timeline
png(here_fig("study-area-timeline.png"),
    height = 5, width = 5, units = "in", res = 600)
time <- terra::deepcopy(map)
time[] <- paths$timestep
expect_true(all(time[] %in% paths$timestep))
terra::plot(time,
            range = range(paths$timestep),
            col = col_path)
dev.off()

#### Visualise the moment model
png(here_fig("model-move.png"),
    height = 3, width = 6, units = "in", res = 600)
pp <- set_par(mfrow = c(1, 2), mar = c(1.5, 1.5, 1.5, 1.5), oma = c(2, 3, 1, 1))
# Step lengths
x <- seq(0, 1000, length.out = 1e5)
y <- dtrunc(x, "gamma", 0, 750, shape = 1, scale = 250)
pretty_plot(x, y,
            pretty_axis_args = paa,
            type = "l")
add_dbn(x, y)
mtext(side = 1, "Step length (m)", line = 2)
mtext(side = 2, "Density", line = 3.5)
# Turning angles
x  <- seq(-pi*1.1, pi*1.1, length.out = 1e5)
y   <- dunif(x, -pi, pi)
pretty_plot(x, y,
            pretty_axis_args = paa,
            type = "l")
add_dbn(x, y)
mtext(side = 1, "Turning angle (rad)", line = 2)
dev.off()

#### Observation models
png(here_fig("model-obs.png"),
    height = 3, width = 6, units = "in", res = 600)
pp <- set_par(mfrow = c(1, 2), mar = c(1.5, 1.5, 1.5, 1.5), oma = c(2, 3, 1, 1))
# Acoustic observations (0, 1)
a <- moorings$receiver_alpha[1]
b <- moorings$receiver_beta[1]
g <- moorings$receiver_gamma[1]
x <- seq(1, 1000, by = 1)
y <- dbinom(1, size = 1, prob = ifelse(x <= g, plogis(a * b * x), 0))
pretty_plot(x, y,
            pretty_axis_args = paa,
            xlab = "", ylab = "",
            type = "l")
add_dbn(x, y)
mtext(side = 1, "Distance (m)", line = 2)
mtext(side = 2, "Density", line = 3.5)
# Archival observations
x <- seq(-25, 25, by = 0.1)
y <- dunif(x, -20, 20)
pretty_plot(x, y,
            pretty_axis_args = paa,
            xlab = "", ylab = "",
            type = "l")
lines(c(0, 0), c(0, max(y)), lty = 3)
add_dbn(x, y)
mtext(side = 1, "Distance (m)", line = 2)
par(pp)
dev.off()

#### Simulated time series
png(here_fig("obs.png"),
    height = 3, width = 6, units = "in", res = 600)
pp <- set_par(mar = c(1.5, 1.5, 1.5, 1.5), oma = c(1, 3, 2, 1))
# Simulated depth time series (x 2)
pretty_plot(archival_perfect$timestep,
            archival_perfect$depth,
            xlab = "", ylab = "",
            xlim = c(0, max(paths$timestep)),
            ylim = c(bathy_zlim),
            pretty_axis_args = list(side = 3:2, pretty = list(n = 3)),
            type = "n")
add_archival(archival_perfect$timestep, archival_perfect$depth, archival_perfect$col)
add_archival(archival_imperfect$timestep, archival_perfect$depth, col = "black", lwd = 0.1,
             type = "p", pch = 21, cex = 0.1, bg = archival_imperfect$col)
# Add detections (red, green)
add_detections(detections_perfect$timestep, max(bathy_zlim))
add_detections(detections_imperfect$timestep, min(bathy_zlim))
mtext(side = 2, "Depth (m)", line = 2)
mtext(side = 3, "Time (step)", line = 2)
par(pp)
dev.off()


#########################
#########################
#### Fig. 3.

#### Approx. caption.
'
Figure 3. patter outputs for the first (A) and second (B–F) worked examples. A is the movement path reconstructed by the particle filter for the first example. Points mark receivers. The simulated movement path is recovered perfectly because the observations exactly define the individual’s location. B–C show particles from the filter and smoother, respectively, at a randomly chosen time step. D maps the pattern of space use over the entire time series using smoothed particles. Each cell represents the probability mass of the individual being in that cell at a randomly chosen time. Lines mark home ranges (and contain 95 % of the probability mass volume). Residency in the blue polygon is XXX. F shows particle diagnostics.
'

#### A: Reconstructed movements
# This code is simply copied from above with minor amendments
paths <- readRDS(here_data("filter", "pff-perfect.rds"))$states
png(here_fig("reconstructed-path.png"),
    height = 5, width = 5, units = "in", res = 600)
# Define region of interest
bb <- terra::ext(map)
xlim <- bb[1:2]
ylim <- bb[3:4]
# Plot map
pp <- set_par()
terra::plot(map,
            col = scales::alpha("lightgrey", 0.25),
            xlim = xlim, ylim = ylim,
            axes = FALSE, legend = FALSE)
axis(side = 1, at = xlim, pos = ylim[1], labels = FALSE)
axis(side = 2, at = ylim, pos = xlim[1], labels = FALSE)
axis(side = 3, at = xlim, pos = ylim[2], labels = FALSE)
axis(side = 4, at = ylim, pos = xlim[2], labels = FALSE)
pretty_axis(side = 1:2,
            lim = list(xlim, ylim),
            pretty = list(n = 3),
            control_sci_notation = list(magnitude = Inf, digits = 0L),
            control_axis = list(las = FALSE, cex.axis = 0.9),
            add = TRUE)
par(pp)
# Add simulated path
col_path <- viridis::inferno(nrow(paths), direction = -1)
spatPoints(paths$x, paths$y, col = NA)
add_sp_path(paths$x, paths$y, length = 0.01, lwd = 0.25,
            col = col_path)
# Add coast
terra::lines(coast, col = "dimgrey", lwd = 0.5)
terra::lines(mpa, col = "darkblue", lwd = 2)
# Add scale bars
terra::sbar(d = 5000, xy = cbind(714500, 6225700), halo = FALSE)
terra::north(d = 4000, xy = cbind(676060.1, 6276000), lwd = 2)
dev.off()

#### B: Particle samples at a selected time step (filter & smoother)
tic()
png(here_fig("particles.png"),
    height = 5, width = 10, units = "in", res = 600)
pp <- set_par(mfrow = c(1, 2))
# pp <- set_par(mfrow = c(1, 3))
t  <- 503L
s1 <- pff_imperfect$states
s1 <- s1[timestep == t, ]
s2 <- tff$states
s2 <- s2[timestep == t, ]
# s3 <- pfb_imperfect$states
# s3 <- s3[timestep == t, ]
xlim <- range(c(s1$x, s2$x))
ylim <- range(c(s1$y, s2$y))
xlim <- square(xlim, ylim)$xlim
ylim <- square(xlim, ylim)$ylim
if (FALSE) {
  # Visualise location
  terra::plot(map)
  terra::plot(terra::ext(c(xlim, ylim)), add = TRUE, lwd = 2)
}
# ss <- list(s1, s3, s2)
ss <- list(s1, s2)
lapply(ss, function(s) {
  # Plot UD
  # s <- s2
  # bw.diggle: ok, insufficient smoothing (~3 s)
  # bw.scott: ok, relatively smooth (~3 s)
  # bw.ppl: better, slow (~42 s)
  # bw.CvL: massive oversmoothing (~3 s)
  bb <- terra::ext(c(xlim, ylim))
  ud <- map_dens(terra::crop(map_ud, bb + 100),
                 .coord = s,
                 sigma = bw.diggle,
                 .plot = FALSE)$ud
  ud <- ud / terra::global(ud, "max", na.rm = TRUE)[1, 1]
  terra::plot(ud,
              xlim = xlim, ylim = ylim,
              smooth = TRUE,
              legend = FALSE,
              axes = FALSE)
  axis(side = 1, at = xlim, pos = ylim[1], labels = FALSE)
  axis(side = 2, at = ylim, pos = xlim[1], labels = FALSE)
  axis(side = 3, at = xlim, pos = ylim[2], labels = FALSE)
  axis(side = 4, at = ylim, pos = xlim[2], labels = FALSE)
  pretty_axis(side = 1:2,
              lim = list(xlim, ylim),
              pretty = list(n = 3),
              control_sci_notation = list(magnitude = Inf, digits = 0L),
              control_axis = list(las = FALSE, cex.axis = 0.9),
              add = TRUE)
  # Add particles
  spatPoints(s$x, s$y,
             pch = ".", col = scales::alpha("black", 0.25))
  # Add true location
  spatPoints(paths$x[t], paths$y[t], col = "black", pch = 4, lwd = 2, cex = 1)
  # Add map components
  terra::sbar(d = 500, halo = FALSE)
  terra::north(d = 500, xy = "topleft", lwd = 2)
  terra::lines(coast, col = "dimgrey", lwd = 0.5)
}) |> invisible()
par(pp)
dev.off()
toc()

#### D: Utilisation distribution & home ranges
# (Map code tweaked from above)
png(here_fig("ud.png"),
    height = 5, width = 5, units = "in", res = 600)
# Define region of interest
bb <- terra::ext(map)
xlim <- bb[1:2]
ylim <- bb[3:4]
# Plot map
pp <- set_par()
ud_raw <- terra::deepcopy(ud)
ud <- ud / terra::global(ud, "max", na.rm = TRUE)[1, 1]
terra::plot(ud, smooth = TRUE,
            axes = FALSE, legend = FALSE)
axis(side = 1, at = xlim, pos = ylim[1], labels = FALSE)
axis(side = 2, at = ylim, pos = xlim[1], labels = FALSE)
axis(side = 3, at = xlim, pos = ylim[2], labels = FALSE)
axis(side = 4, at = ylim, pos = xlim[2], labels = FALSE)
pretty_axis(side = 1:2,
            lim = list(xlim, ylim),
            pretty = list(n = 3),
            control_sci_notation = list(magnitude = Inf, digits = 0L),
            control_axis = list(las = FALSE, cex.axis = 0.9),
            add = TRUE)
par(pp)
# Add home range
map_hr_core(ud_raw, .add = TRUE, lwd = 0.5)
# map_hr_home(ud_raw, .add = TRUE, lwd = 0.5)
# Add coast
terra::lines(coast, col = "dimgrey", lwd = 0.5)
terra::lines(mpa, col = "darkblue", lwd = 2)
# Add scale bars
terra::sbar(d = 5000, xy = cbind(714500, 6225700), halo = FALSE)
terra::north(d = 4000, xy = cbind(676060.1, 6276000), lwd = 2)
dev.off()

#### E-F: Particle diagnostics
# > See workflow.R


#### End of code.
#########################
#########################
