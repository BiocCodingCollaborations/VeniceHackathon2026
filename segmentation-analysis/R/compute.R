library(jsonlite)
library(Rarr)

compute <- function(path,
                    image = "morphology_focus",
                    level = 0,
                    shapes = "cell_boundaries",
                    channel = 1,
                    id_col = "cell_id") {
  image_group <- jsonlite::fromJSON(
    file.path(path, "images", image, "zarr.json"),
    simplifyVector = FALSE
  )
  image_array <- jsonlite::fromJSON(
    file.path(path, "images", image, as.character(level), "zarr.json"),
    simplifyVector = FALSE
  )

  dataset_scale <- unlist(
    image_group$attributes$ome$multiscales[[1]]$datasets[[level + 1]]$coordinateTransformations[[1]]$scale
  )
  dataset_scale <- dataset_scale[-1]

  transform_seq <- image_group$attributes$ome$multiscales[[1]]$coordinateTransformations[[1]]$transformations
  scale_y <- dataset_scale[1]
  scale_x <- dataset_scale[2]
  offset_y <- 0
  offset_x <- 0

  for (tr in transform_seq) {
    if (tr$type == "scale") {
      scale_y <- scale_y * tr$scale[[2]]
      scale_x <- scale_x * tr$scale[[3]]
      offset_y <- offset_y * tr$scale[[2]]
      offset_x <- offset_x * tr$scale[[3]]
    }

    if (tr$type == "translation") {
      offset_y <- offset_y + tr$translation[[2]]
      offset_x <- offset_x + tr$translation[[3]]
    }
  }

  bytes <- image_array$codecs[[1]]$configuration
  type <- switch(image_array$data_type,
    uint8 = list(base_type = "uint", nbytes = 1L, is_signed = FALSE),
    uint16 = list(base_type = "uint", nbytes = 2L, is_signed = FALSE),
    uint32 = list(base_type = "uint", nbytes = 4L, is_signed = FALSE),
    int8 = list(base_type = "int", nbytes = 1L, is_signed = TRUE),
    int16 = list(base_type = "int", nbytes = 2L, is_signed = TRUE),
    int32 = list(base_type = "int", nbytes = 4L, is_signed = TRUE),
    float32 = list(base_type = "float", nbytes = 4L, is_signed = TRUE),
    float64 = list(base_type = "float", nbytes = 8L, is_signed = TRUE)
  )

  metadata <- list(
    shape = as.integer(unlist(image_array$shape)),
    chunks = as.integer(unlist(image_array$chunk_grid$configuration$chunk_shape)),
    fill_value = as.integer(image_array$fill_value),
    order = "C",
    dimension_separator = image_array$chunk_key_encoding$configuration$separator,
    compressor = list(id = image_array$codecs[[2]]$name),
    datatype = c(list(endian = bytes$endian), type)
  )

  check_index <- getFromNamespace("check_index", "Rarr")
  find_chunks_needed <- getFromNamespace("find_chunks_needed", "Rarr")
  read_data <- getFromNamespace("read_data", "Rarr")
  chunk_path <- file.path(path, "images", image, as.character(level), "c", "")

  y_shape <- metadata$shape[2]
  x_shape <- metadata$shape[3]
  y_chunk <- metadata$chunks[2]
  x_chunk <- metadata$chunks[3]

  y_starts <- seq.int(1L, y_shape, by = y_chunk)
  x_starts <- seq.int(1L, x_shape, by = x_chunk)
  total_patches <- length(y_starts) * length(x_starts)
  out <- vector("list", total_patches)
  k <- 0L
  patch_i <- 0L
  pb <- utils::txtProgressBar(min = 0, max = total_patches, style = 3)
  on.exit(close(pb), add = TRUE)

  for (y0 in y_starts) {
    for (x0 in x_starts) {
      patch_i <- patch_i + 1L
      utils::setTxtProgressBar(pb, patch_i)

      y1 <- min(y0 + y_chunk - 1L, y_shape)
      x1 <- min(x0 + x_chunk - 1L, x_shape)

      minx <- offset_x + (x0 - 1) * scale_x
      miny <- offset_y + (y0 - 1) * scale_y
      maxx <- offset_x + x1 * scale_x
      maxy <- offset_y + y1 * scale_y

      shp <- read_geoparquet(
        file.path(path, "shapes", shapes),
        minx = minx,
        miny = miny,
        maxx = maxx,
        maxy = maxy
      )

      shp <- shp[
        shp$minx >= minx &
          shp$miny >= miny &
          shp$maxx <= maxx &
          shp$maxy <= maxy, ,
        drop = FALSE
      ]

      if (nrow(shp) == 0) {
        next
      }

      attr(shp, "minx") <- minx
      attr(shp, "miny") <- miny
      attr(shp, "maxx") <- maxx
      attr(shp, "maxy") <- maxy
      attr(shp, "scale_factor") <- scale_x

      labels <- shapes_to_labels(shp, start_id = 1L)

      index <- list(
        as.integer(channel),
        as.integer(seq.int(y0, y1)),
        as.integer(seq.int(x0, x1))
      )
      index <- check_index(index, metadata)
      needed <- as.matrix(find_chunks_needed(metadata, index))
      chunk <- read_data(needed, chunk_path, NULL, index, metadata)$output
      chunk <- drop(chunk)

      props <- regionprops(labels, chunk)
      props[[id_col]] <- shp[[id_col]][props$label]
      props$chunk_y0 <- y0
      props$chunk_x0 <- x0
      props$global_centroid_y <- miny + props$centroid.0 * scale_y
      props$global_centroid_x <- minx + props$centroid.1 * scale_x
      props$global_bbox_miny <- miny + props$bbox.0 * scale_y
      props$global_bbox_minx <- minx + props$bbox.1 * scale_x
      props$global_bbox_maxy <- miny + props$bbox.2 * scale_y
      props$global_bbox_maxx <- minx + props$bbox.3 * scale_x

      k <- k + 1L
      out[[k]] <- props
    }
  }

  do.call(rbind, out[seq_len(k)])
}
