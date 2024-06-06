#' A fast(er) `terra::extract()` function

fextract <- function(.x, .coord, .col, .res = NULL) {

  tictoc::tic()
  on.exit(tictoc::toc(), add = TRUE)

  #### Define data.table
  .coord <- data.table(x = .coord[, 1], y = .coord[, 2])

  #### Define grid cells
  if (inherits(.x, "SpatRaster")) {
    .coord[, id := terra::cellFromXY(.x, cbind(x, y))]
  } else if (inherits(.x, "SpatVector")) {
    # (optional) Round coordinates
    if (!is.null(.res)) {
      .coord[, x := plyr::round_any(x, .res)]
      .coord[, y := plyr::round_any(y, .res)]
    }
    .coord[, id := .GRP, by = list(x, y)]
  } else {
    rlang::abort("`.x` must be a `SpatRaster` or a `SpatVector`.")
  }

  #### Define unique cells/coordinates
  xy <-
    .coord |>
    lazy_dt(immutable = TRUE) |>
    dplyr::select("id", "x", "y") |>
    distinct(id, .keep_all = TRUE) |>
    as.data.table()

  #### Implement `terra::extract()` for unique coordinates only
  # TO DO
  # If .x inherits a SpatRaster, use xy$id rather than coordinates
  x <- y <- z <- NULL
  xy[, z := terra::extract(.x, cbind(xy$x, xy$y))[[.col]]]

  #### Match values back onto `.coord`
  .coord[, z := xy$z[fmatch(id, xy$id)]]
  .coord$z
}
