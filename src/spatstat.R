#' Create an observation window for the Firth of Lorn
#' .poly (sf coastline object) is cropped to .map or to range(.x) and range(.y) + buffer if supplied

as.owin.fol <- function(.poly, .map, .x = NULL, .y = NULL, .buffer = 2000, .plot = TRUE) {

  tictoc::tic()
  on.exit(tictoc::toc(), add = TRUE)

  # Define SpatVector
  .poly <- terra::vect(sf::st_geometry(.poly))

  # Define box for cropping
  bb <- bbb <- terra::ext(.map)
  if (!is.null(.x) & !is.null(.y)) {
    bb <- terra::ext(c(range(.x), range(.y)))
    bbb <- bb + .buffer
  }

  # Crop poly to box
  .poly <- terra::crop(.poly, bbb)

  # Compute window
  .poly <- sf::st_as_sf(.poly)
  win   <- as.owin.sf(.poly, .invert = TRUE)

  # Visual check
  # * NB: par() does not work properly here
  if (.plot) {
    pp <- par(mfrow = c(1, 2))
    on.exit(par(pp, no.readonly = TRUE), add = TRUE)
    terra::plot(.poly)
    terra::plot(.map, add = TRUE)
    terra::lines(.poly)
    plot(win, col = "blue")
    rect(xleft = bb[1], xright = bb[2],
         ybottom = bb[3], ytop = bb[4])
  }

  # Return window
  win

}
