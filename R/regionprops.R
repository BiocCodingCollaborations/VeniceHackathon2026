compute_eccentricity <- function(rows, cols) {
  rr <- rows - 1
  cc <- cols - 1

  mu_rr <- mean(rr)
  mu_cc <- mean(cc)

  x <- cc - mu_cc
  y <- rr - mu_rr

  cov_mat <- matrix(
    c(mean(x * x), mean(x * y), mean(x * y), mean(y * y)),
    nrow = 2,
    byrow = TRUE
  )

  eig <- eigen(cov_mat, symmetric = TRUE, only.values = TRUE)$values
  major <- eig[1]
  minor <- eig[2]

  sqrt(1 - (minor / major))
}

regionprops <- function(labels, intensity = labels > 0) {
  ids <- sort(unique(as.vector(labels)))
  ids <- ids[ids > 0]

  out <- lapply(ids, function(id) {
    idx <- which(labels == id, arr.ind = TRUE)
    rows <- idx[, 1]
    cols <- idx[, 2]

    data.frame(
      label = id,
      area = nrow(idx),
      centroid.0 = mean(rows - 1),
      centroid.1 = mean(cols - 1),
      bbox.0 = min(rows) - 1,
      bbox.1 = min(cols) - 1,
      bbox.2 = max(rows),
      bbox.3 = max(cols),
      eccentricity = compute_eccentricity(rows, cols),
      mean_intensity = mean(intensity[labels == id], na.rm = TRUE)
    )
  })

  do.call(rbind, out)
}
