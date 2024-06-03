#' Set par parameters

set_par <- function(...) {
  par(mgp = c(3, 0.25, 0), tcl = -0.25, ...)
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
