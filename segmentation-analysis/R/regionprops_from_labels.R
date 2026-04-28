safe_divide <- function(x, y) {
  if (isTRUE(all.equal(y, 0))) {
    return(0)
  }

  x / y
}

polygon_area <- function(x, y) {
  if (length(x) < 3) {
    return(0)
  }

  x2 <- c(x, x[1])
  y2 <- c(y, y[1])
  abs(sum(x2[-1] * y2[-length(y2)] - x2[-length(x2)] * y2[-1])) / 2
}

point_in_polygon <- function(px, py, vx, vy) {
  n <- length(vx)
  inside <- FALSE
  j <- n

  for (i in seq_len(n)) {
    yi <- vy[i]
    yj <- vy[j]
    xi <- vx[i]
    xj <- vx[j]

    intersects <- ((yi > py) != (yj > py)) &&
      (px < (xj - xi) * (py - yi) / ((yj - yi) + 1e-12) + xi)

    if (intersects) {
      inside <- !inside
    }

    j <- i
  }

  inside
}

fill_polygon_mask <- function(vx, vy, nrow_mask, ncol_mask) {
  out <- matrix(FALSE, nrow_mask, ncol_mask)

  for (r in seq_len(nrow_mask)) {
    for (c in seq_len(ncol_mask)) {
      out[r, c] <- point_in_polygon(c - 1, r - 1, vx, vy)
    }
  }

  out
}

label_components <- function(x, target = TRUE, connectivity = 4) {
  nr <- nrow(x)
  nc <- ncol(x)
  labels <- matrix(0L, nr, nc)
  current <- 0L

  dirs <- if (connectivity == 8) {
    rbind(
      c(-1L, 0L), c(1L, 0L), c(0L, -1L), c(0L, 1L),
      c(-1L, -1L), c(-1L, 1L), c(1L, -1L), c(1L, 1L)
    )
  } else {
    rbind(c(-1L, 0L), c(1L, 0L), c(0L, -1L), c(0L, 1L))
  }

  for (r in seq_len(nr)) {
    for (c in seq_len(nc)) {
      if (isTRUE(x[r, c] == target) && labels[r, c] == 0L) {
        current <- current + 1L
        queue_r <- integer(length = nr * nc)
        queue_c <- integer(length = nr * nc)
        head <- 1L
        tail <- 1L
        queue_r[tail] <- r
        queue_c[tail] <- c
        labels[r, c] <- current

        while (head <= tail) {
          rr <- queue_r[head]
          cc <- queue_c[head]
          head <- head + 1L

          for (k in seq_len(nrow(dirs))) {
            nr2 <- rr + dirs[k, 1]
            nc2 <- cc + dirs[k, 2]

            if (nr2 >= 1L && nr2 <= nr && nc2 >= 1L && nc2 <= nc) {
              if (isTRUE(x[nr2, nc2] == target) && labels[nr2, nc2] == 0L) {
                tail <- tail + 1L
                queue_r[tail] <- nr2
                queue_c[tail] <- nc2
                labels[nr2, nc2] <- current
              }
            }
          }
        }
      }
    }
  }

  labels
}

fill_holes <- function(image) {
  background <- !image
  comps <- label_components(background, target = TRUE, connectivity = 4)
  border_ids <- unique(c(
    comps[1, ],
    comps[nrow(comps), ],
    comps[, 1],
    comps[, ncol(comps)]
  ))
  holes <- background & !(comps %in% border_ids)
  image | holes
}

convex_hull_image <- function(image, rows_local0, cols_local0) {
  if (length(rows_local0) < 3) {
    return(image)
  }

  hull_idx <- grDevices::chull(cols_local0, rows_local0)
  hull_x <- cols_local0[hull_idx]
  hull_y <- rows_local0[hull_idx]
  fill_polygon_mask(hull_x, hull_y, nrow(image), ncol(image))
}

feret_diameter_max <- function(rows0, cols0) {
  if (length(rows0) < 2) {
    return(0)
  }

  hull_idx <- grDevices::chull(cols0, rows0)
  pts <- cbind(rows0[hull_idx], cols0[hull_idx])
  d <- as.matrix(dist(pts))
  max(d)
}

perimeter_4 <- function(image) {
  nr <- nrow(image)
  nc <- ncol(image)
  p <- 0

  for (r in seq_len(nr)) {
    for (c in seq_len(nc)) {
      if (image[r, c]) {
        if (r == 1 || !image[r - 1, c]) p <- p + 1
        if (r == nr || !image[r + 1, c]) p <- p + 1
        if (c == 1 || !image[r, c - 1]) p <- p + 1
        if (c == nc || !image[r, c + 1]) p <- p + 1
      }
    }
  }

  p
}

perimeter_crofton_4 <- function(image) {
  nr <- nrow(image)
  nc <- ncol(image)
  n0 <- 0
  n90 <- 0
  n45 <- 0
  n135 <- 0

  for (r in seq_len(nr)) {
    for (c in seq_len(nc - 1L)) {
      n0 <- n0 + xor(image[r, c], image[r, c + 1L])
    }
  }

  for (r in seq_len(nr - 1L)) {
    for (c in seq_len(nc)) {
      n90 <- n90 + xor(image[r, c], image[r + 1L, c])
    }
  }

  for (r in seq_len(nr - 1L)) {
    for (c in seq_len(nc - 1L)) {
      n45 <- n45 + xor(image[r, c], image[r + 1L, c + 1L])
      n135 <- n135 + xor(image[r + 1L, c], image[r, c + 1L])
    }
  }

  (pi / 4) * (n0 + n90 + (n45 + n135) / sqrt(2))
}

raw_moments <- function(rows0, cols0, weights, order = 3) {
  out <- matrix(0, order + 1, order + 1)

  for (i in 0:order) {
    for (j in 0:order) {
      out[i + 1, j + 1] <- sum(weights * (rows0^i) * (cols0^j))
    }
  }

  out
}

central_moments <- function(rows0, cols0, weights, center_row, center_col, order = 3) {
  out <- matrix(0, order + 1, order + 1)

  for (i in 0:order) {
    for (j in 0:order) {
      out[i + 1, j + 1] <- sum(
        weights * ((rows0 - center_row)^i) * ((cols0 - center_col)^j)
      )
    }
  }

  out
}

normalized_moments <- function(mu) {
  out <- matrix(0, nrow(mu), ncol(mu))
  m00 <- mu[1, 1]

  for (i in 0:(nrow(mu) - 1L)) {
    for (j in 0:(ncol(mu) - 1L)) {
      if (i + j >= 2) {
        out[i + 1, j + 1] <- safe_divide(
          mu[i + 1, j + 1],
          m00^(((i + j) / 2) + 1)
        )
      }
    }
  }

  out
}

hu_moments <- function(nu) {
  n20 <- nu[3, 1]
  n02 <- nu[1, 3]
  n11 <- nu[2, 2]
  n30 <- nu[4, 1]
  n12 <- nu[2, 3]
  n21 <- nu[3, 2]
  n03 <- nu[1, 4]

  c(
    n20 + n02,
    (n20 - n02)^2 + 4 * (n11^2),
    (n30 - 3 * n12)^2 + (3 * n21 - n03)^2,
    (n30 + n12)^2 + (n21 + n03)^2,
    (n30 - 3 * n12) * (n30 + n12) * ((n30 + n12)^2 - 3 * (n21 + n03)^2) +
      (3 * n21 - n03) * (n21 + n03) * (3 * (n30 + n12)^2 - (n21 + n03)^2),
    (n20 - n02) * ((n30 + n12)^2 - (n21 + n03)^2) +
      4 * n11 * (n30 + n12) * (n21 + n03),
    (3 * n21 - n03) * (n30 + n12) * ((n30 + n12)^2 - 3 * (n21 + n03)^2) -
      (n30 - 3 * n12) * (n21 + n03) * (3 * (n30 + n12)^2 - (n21 + n03)^2)
  )
}

make_row <- function(x) {
  as.data.frame(x, stringsAsFactors = FALSE)
}

regionprops <- function(labels, intensity, spacing = c(1, 1), offset = c(0, 0)) {
  ids <- sort(unique(as.vector(labels)))
  ids <- ids[ids > 0]
  pixel_area <- prod(spacing)

  out <- lapply(ids, function(id) {
    idx <- which(labels == id, arr.ind = TRUE)
    rows1 <- idx[, 1]
    cols1 <- idx[, 2]
    rows0 <- rows1 - 1
    cols0 <- cols1 - 1
    values <- intensity[labels == id]

    min_row1 <- min(rows1)
    min_col1 <- min(cols1)
    max_row1 <- max(rows1)
    max_col1 <- max(cols1)

    bbox <- c(min_row1 - 1, min_col1 - 1, max_row1, max_col1)
    bbox_nrow <- bbox[3] - bbox[1]
    bbox_ncol <- bbox[4] - bbox[2]

    rows_local0 <- rows1 - min_row1
    cols_local0 <- cols1 - min_col1

    image <- matrix(FALSE, bbox_nrow, bbox_ncol)
    image[cbind(rows_local0 + 1, cols_local0 + 1)] <- TRUE

    intensity_image <- matrix(0, bbox_nrow, bbox_ncol)
    intensity_image[cbind(rows_local0 + 1, cols_local0 + 1)] <- values

    image_filled <- fill_holes(image)
    image_convex <- convex_hull_image(image, rows_local0, cols_local0)

    filled_count <- sum(image_filled)
    convex_count <- sum(image_convex)
    area_count <- nrow(idx)

    centroid_local <- c(mean(rows_local0), mean(cols_local0))
    centroid <- c(mean(rows0) + offset[1], mean(cols0) + offset[2])

    w_sum <- sum(values)
    centroid_weighted_local <- c(
      safe_divide(sum(rows_local0 * values), w_sum),
      safe_divide(sum(cols_local0 * values), w_sum)
    )
    centroid_weighted <- c(
      safe_divide(sum(rows0 * values), w_sum) + offset[1],
      safe_divide(sum(cols0 * values), w_sum) + offset[2]
    )

    m <- raw_moments(rows_local0, cols_local0, rep(1, length(rows_local0)))
    mu <- central_moments(
      rows_local0,
      cols_local0,
      rep(1, length(rows_local0)),
      centroid_local[1],
      centroid_local[2]
    )
    nu <- normalized_moments(mu)
    hu <- hu_moments(nu)

    wm <- raw_moments(rows_local0, cols_local0, values)
    wmu <- central_moments(
      rows_local0,
      cols_local0,
      values,
      centroid_weighted_local[1],
      centroid_weighted_local[2]
    )
    wnu <- normalized_moments(wmu)
    whu <- hu_moments(wnu)

    cov_mat <- matrix(
      c(
        safe_divide(mu[3, 1], mu[1, 1]),
        safe_divide(mu[2, 2], mu[1, 1]),
        safe_divide(mu[2, 2], mu[1, 1]),
        safe_divide(mu[1, 3], mu[1, 1])
      ),
      nrow = 2,
      byrow = TRUE
    )

    eig <- eigen(cov_mat, symmetric = TRUE)
    eigvals <- eig$values
    major <- 4 * sqrt(max(eigvals, 0))
    minor <- 4 * sqrt(max(min(eigvals), 0))
    eccentricity <- if (major == 0) 0 else sqrt(1 - (minor^2 / major^2))
    orientation <- 0.5 * atan2(2 * cov_mat[1, 2], cov_mat[1, 1] - cov_mat[2, 2])

    image_components <- label_components(image, target = TRUE, connectivity = 4)
    n_components <- length(unique(image_components[image_components > 0]))
    background_components <- label_components(!image, target = TRUE, connectivity = 4)
    border_ids <- unique(c(
      background_components[1, ],
      background_components[nrow(background_components), ],
      background_components[, 1],
      background_components[, ncol(background_components)]
    ))
    all_bg_ids <- unique(background_components[background_components > 0])
    holes <- setdiff(all_bg_ids, border_ids)
    euler_number <- n_components - length(holes)

    row <- list(
      label = id,
      area = area_count * pixel_area,
      area_bbox = bbox_nrow * bbox_ncol * pixel_area,
      area_convex = convex_count * pixel_area,
      area_filled = filled_count * pixel_area,
      axis_major_length = major,
      axis_minor_length = minor,
      bbox.0 = bbox[1],
      bbox.1 = bbox[2],
      bbox.2 = bbox[3],
      bbox.3 = bbox[4],
      centroid.0 = centroid[1],
      centroid.1 = centroid[2],
      centroid_local.0 = centroid_local[1],
      centroid_local.1 = centroid_local[2],
      centroid_weighted.0 = centroid_weighted[1],
      centroid_weighted.1 = centroid_weighted[2],
      centroid_weighted_local.0 = centroid_weighted_local[1],
      centroid_weighted_local.1 = centroid_weighted_local[2],
      coords = I(list(cbind(rows0 + offset[1], cols0 + offset[2]))),
      coords_scaled = I(list(cbind(rows0 * spacing[1], cols0 * spacing[2]))),
      eccentricity = eccentricity,
      equivalent_diameter_area = sqrt(4 * area_count * pixel_area / pi),
      euler_number = euler_number,
      extent = safe_divide(area_count, bbox_nrow * bbox_ncol),
      feret_diameter_max = feret_diameter_max(rows0, cols0),
      image = I(list(image)),
      image_convex = I(list(image_convex)),
      image_filled = I(list(image_filled)),
      image_intensity = I(list(intensity_image)),
      inertia_tensor = I(list(cov_mat)),
      inertia_tensor_eigvals = I(list(eigvals)),
      intensity_max = max(values),
      intensity_mean = mean(values),
      intensity_median = stats::median(values),
      intensity_min = min(values),
      intensity_std = stats::sd(values),
      moments = I(list(m)),
      moments_central = I(list(mu)),
      moments_hu = I(list(hu)),
      moments_normalized = I(list(nu)),
      moments_weighted = I(list(wm)),
      moments_weighted_central = I(list(wmu)),
      moments_weighted_hu = I(list(whu)),
      moments_weighted_normalized = I(list(wnu)),
      num_pixels = area_count,
      orientation = orientation,
      perimeter = perimeter_4(image),
      perimeter_crofton = perimeter_crofton_4(image),
      slice = I(list(list(rows = c(bbox[1], bbox[3]), cols = c(bbox[2], bbox[4])))),
      solidity = safe_divide(area_count, convex_count)
    )

    make_row(row)
  })

  do.call(rbind, out)
}
