#' Add distribution polygons

add_dbn <- function(x, y,
                    col = scales::alpha("lightgrey", 0.25), ...) {
  polygon(c(x, rev(x)), c(y, rep(0, length(y))), col = col, ...)
  invisible(NULL)
}

#' Add archival time series

add_archival <- function(x, y, col) {
  n <- length(x)
  y <- abs(y) * -1
  arrows(x0 = x[1:(n - 1)],
         x1 = x[2:n],
         y0 = y[1:(n - 1)],
         y1 =  y[2:n],
         col = col,
         length = 0,
         lwd = 3)
  invisible(NULL)
}

#' Add detections

add_detections <- function(x, y = 0, col = "darkgreen") {
  points(x,
         rep(y, length(x)),
         pch = 21, col = col, bg = col)
}
