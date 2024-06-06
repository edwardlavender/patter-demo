#' Set par parameters

set_par <- function(...) {
  par(mgp = c(3, 0.25, 0), tcl = -0.25, ...)
}

#' Add points to a map

spatPoints <- function(x, y, ...) {
  # Use terra::plot() to add points to a map for safety
  # (with points(), points may be misplaced when par() is set)
  terra::plot(terra::vect(cbind(x, y), crs = "EPSG:32629"),
              ..., add = TRUE)
}

#' Add distribution polygons

add_dbn <- function(x, y,
                    col = scales::alpha("lightgrey", 0.25), ...) {
  polygon(c(x, rev(x)), c(y, rep(0, length(y))), col = col, ...)
  invisible(NULL)
}

#' Add archival time series

add_archival <- function(x, y, col, type = c("a", "p"), ...) {
  type <- match.arg(type)
  if (type == "a") {
    n <- length(x)
    y <- abs(y) * -1
    arrows(x0 = x[1:(n - 1)],
           x1 = x[2:n],
           y0 = y[1:(n - 1)],
           y1 =  y[2:n],
           col = col,
           length = 0,
           ...)
  } else if (type == "p") {
    points(x, y, col = col, ...)
  }
  invisible(NULL)
}

#' Add detections

add_detections <- function(x, y = 0, col = "darkgreen") {
  points(x,
         rep(y, length(x)),
         pch = 21, col = col, bg = col)
}

#' Make plot limits square (for pretty maps)

square <- function(xlim, ylim) {
  x_range   <- diff(xlim)
  y_range   <- diff(ylim)
  max_range <- max(c(x_range, y_range))
  xlim      <- mean(xlim) + c(-0.5, 0.5) * max_range
  ylim      <- mean(ylim) + c(-0.5, 0.5) * max_range
  list(xlim = xlim, ylim = ylim)
}

#' Add gridlines around raster cells

spatGrid <- function(x, ...) {
  # Define grid line positions
  r <- terra::res(x)
  h <- r[1]
  v <- r[2]
  bb <- terra::ext(x)
  xc <- seq(bb[1], bb[2], by = h)
  yc <- seq(bb[3], bb[4], by = v)
  # Add vertical grid lines
  for (i in seq_len(length(xc))) {
    lines(rep(xc[i], 2), bb[3:4], ...)
  }
  # Add horizontal grid lines
  for (i in seq_len(length(yc))) {
    lines(bb[1:2], rep(yc[i], 2), ...)
  }
  invisible(NULL)
}
