library(sf)
library(terra)

shapes_to_labels <- function(shapes, start_id = 1) {
  minx <- attr(shapes, "minx")
  miny <- attr(shapes, "miny")
  maxx <- attr(shapes, "maxx")
  maxy <- attr(shapes, "maxy")
  scale_factor <- attr(shapes, "scale_factor")
  ncols <- (maxx - minx) / scale_factor
  nrows <- (maxy - miny) / scale_factor

  if (nrow(shapes) == 0) {
    return(matrix(0, nrows, ncols))
  }

  geom <- sf::st_as_sfc(shapes$geometry, EWKB = TRUE)
  geom <- (geom - c(minx, miny)) * (1 / scale_factor) + c(0, 0)
  shapes <- sf::st_sf(label = start_id - 1 + seq_len(nrow(shapes)), geometry = geom)
  template <- terra::rast(nrows = nrows, ncols = ncols, xmin = 0, xmax = ncols, ymin = 0, ymax = nrows)

  label_raster <- terra::rasterize(
    terra::vect(shapes),
    template,
    field = "label",
    touches = TRUE,
    background = 0
  )

  terra::as.matrix(label_raster, wide = TRUE)
}
