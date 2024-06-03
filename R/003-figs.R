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
library(prettyGraphics)
library(truncdist)
dv::src()
add_sp_path <- patter:::add_sp_path

#### Load data
map <- terra::rast(here_data("spatial", "map-negative.tif"))
mpa   <- terra::vect(readRDS(here_data("spatial", "mpa.rds")))
coast <- readRDS(here_data("spatial", "coast.rds"))
paths      <- readRDS(here_data("sim", "paths.rds"))
xinit      <- readRDS(here_data("sim", "xinit.rds"))
moorings   <- readRDS(here_data("sim", "moorings.rds"))
detections_perfect   <- readRDS(here_data("sim", "detections-perfect.rds"))
detections_imperfect <- readRDS(here_data("sim", "detections-imperfect.rds"))
archival_perfect     <- readRDS(here_data("sim", "archival-perfect.rds"))
archival_imperfect   <- readRDS(here_data("sim", "archival-imperfect.rds"))


#########################
#########################
#### Graphical settings

#### Pretty axes
paa <- list(pretty = list(n = 3))

#### Bathymetry colour scheme
# Bathymetry colour scheme
bathy_zlim <- c(-225, 0)
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
moorings[, col := ifelse(moorings$receiver_id %in%
                           c(detections_perfect$perfect,
                             detections_imperfect$sensor_id),
                             yes = "darkgreen",
                             no = "darkred")]


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
terra::plot(map,
            col = bathy_cols,
            xlim = xlim, ylim = ylim,
            range = bathy_zlim,
            axes = FALSE)
axis(side = 1, at = xlim, pos = ylim[1], labels = FALSE)
axis(side = 2, at = ylim, pos = xlim[1], labels = FALSE)
axis(side = 3, at = xlim, pos = ylim[2], labels = FALSE)
axis(side = 4, at = ylim, pos = xlim[2], labels = FALSE)
pretty_axis(side = 1:2, lim = list(xlim, ylim), pretty = list(n = 3))
# Add simulated path
add_sp_path(paths$x, paths$y, length = 0.01, lwd = 0.25,
            col = viridis::inferno(nrow(paths)))
# Add coast
terra::lines(coast, col = "dimgrey", lwd = 0.5)
terra::lines(mpa, col = "darkblue", lwd = 2)
# Add receivers & orgin
points(moorings$receiver_x, moorings$receiver_y,
       pch = 21, col = moorings$col, bg = moorings$col, cex = 0.75)
points(xinit$x, xinit$y, col = "black", pch = 4, lwd = 3)
# Add scale bars
terra::sbar(d = 5000, xy = cbind(676147.5, 6226688), halo = FALSE)
terra::north(d = 4000, xy = cbind(676060.1, 6276000), lwd = 2)
dev.off()

#### Visualise the moment model
png(here_fig("model-move.png"),
    height = 3, width = 6, units = "in", res = 600)
pp <- par(mfrow = c(1, 2), mar = c(1.5, 1.5, 1.5, 1.5), oma = c(2, 3, 1, 1))
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
pp <- par(mfrow = c(1, 2), mar = c(1.5, 1.5, 1.5, 1.5), oma = c(2, 3, 1, 1))
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
add_dbn(x, y)
mtext(side = 1, "Distance (m)", line = 2)
par(pp)
dev.off()

#### Simulated time series
png(here_fig("obs.png"),
    height = 3, width = 6, units = "in", res = 600)

pp <- par(mar = c(1.5, 1.5, 1.5, 1.5), oma = c(1, 3, 2, 1))

# Simulated depth time series (x 2)
pretty_plot(archival_perfect$timestep,
            archival_perfect$depth,
            xlab = "", ylab = "",
            xlim = c(0, max(paths$timestep)),
            ylim = c(bathy_zlim),
            pretty_axis_args = list(side = 3:2, pretty = list(n = 3)),
            type = "n")
add_archival(archival_perfect$timestep, archival_perfect$depth, archival$col)
add_archival(archival_imperfect$timestep, archival_perfect$depth, archival$col)
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

#### B: Particle samples at a selected time step (filter)

#### C: Particle samples at a selected time step (smoother)

#### D: Utilisation distribution & home ranges

#### E-F: Particle diagnostics



#### End of code.
#########################
#########################
