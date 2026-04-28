library(DBI)
library(duckdb)
library(jsonlite)
library(sf)

.read_geoparquet_cache <- new.env(parent = emptyenv())
.read_geoparquet_cache$con <- NULL

read_geoparquet_connection <- function() {
  if (is.null(.read_geoparquet_cache$con) || !DBI::dbIsValid(.read_geoparquet_cache$con)) {
    .read_geoparquet_cache$con <- DBI::dbConnect(duckdb::duckdb())
  }

  .read_geoparquet_cache$con
}

read_geoparquet_scale_factor <- function(path) {
  meta <- jsonlite::fromJSON(file.path(path, "zarr.json"), simplifyVector = FALSE)
  transform <- meta$attributes$coordinateTransformations[[1]]

  if (transform$type == "scale") {
    unlist(transform$scale)[1]
  } else {
    1
  }
}

read_geoparquet <- function(path, minx = -1e308, miny = -1e308, maxx = 1e308, maxy = 1e308) {
  con <- read_geoparquet_connection()
  scale_factor <- read_geoparquet_scale_factor(path)
  parquet_path <- file.path(path, "shapes.parquet")

  cols <- DBI::dbGetQuery(
    con,
    paste0(
      "DESCRIBE SELECT * ",
      "FROM read_parquet('", parquet_path, "')"
    )
  )$column_name

  if ("radius" %in% cols) {
    sql <- paste0(
      "WITH shp AS (",
      "SELECT *, ",
      "CAST(split_part(replace(replace(CAST(geometry AS VARCHAR), 'POINT (', ''), ')', ''), ' ', 1) AS DOUBLE) AS px, ",
      "CAST(split_part(replace(replace(CAST(geometry AS VARCHAR), 'POINT (', ''), ')', ''), ' ', 2) AS DOUBLE) AS py ",
      "FROM read_parquet('", parquet_path, "')",
      ") ",
      "SELECT * EXCLUDE (px, py), ",
      "(px - radius) * ", scale_factor, " AS minx, ",
      "(py - radius) * ", scale_factor, " AS miny, ",
      "(px + radius) * ", scale_factor, " AS maxx, ",
      "(py + radius) * ", scale_factor, " AS maxy ",
      "FROM shp ",
      "WHERE (px + radius) * ", scale_factor, " >= ", minx, " ",
      "AND (px - radius) * ", scale_factor, " <= ", maxx, " ",
      "AND (py + radius) * ", scale_factor, " >= ", miny, " ",
      "AND (py - radius) * ", scale_factor, " <= ", maxy
    )
  } else {
    sql <- paste0(
      "WITH shp AS (",
      "SELECT row_number() OVER () AS row_id, * ",
      "FROM read_parquet('", parquet_path, "')",
      "), pts AS (",
      "SELECT row_id, ",
      "unnest(string_split(replace(replace(replace(CAST(geometry AS VARCHAR), 'POLYGON ((', ''), '))', ''), ',', '|'), '|')) AS pt ",
      "FROM shp",
      "), bbox AS (",
      "SELECT row_id, ",
      "min(CAST(split_part(trim(pt), ' ', 1) AS DOUBLE)) * ", scale_factor, " AS minx, ",
      "min(CAST(split_part(trim(pt), ' ', 2) AS DOUBLE)) * ", scale_factor, " AS miny, ",
      "max(CAST(split_part(trim(pt), ' ', 1) AS DOUBLE)) * ", scale_factor, " AS maxx, ",
      "max(CAST(split_part(trim(pt), ' ', 2) AS DOUBLE)) * ", scale_factor, " AS maxy ",
      "FROM pts ",
      "GROUP BY row_id",
      ") ",
      "SELECT shp.* EXCLUDE (row_id), bbox.minx, bbox.miny, bbox.maxx, bbox.maxy ",
      "FROM shp JOIN bbox USING (row_id) ",
      "WHERE bbox.maxx >= ", minx, " ",
      "AND bbox.minx <= ", maxx, " ",
      "AND bbox.maxy >= ", miny, " ",
      "AND bbox.miny <= ", maxy
    )
  }

  out <- DBI::dbGetQuery(con, sql)

  if (nrow(out) > 0) {
    geom <- sf::st_as_sfc(out$geometry, EWKB = TRUE)
    geom <- (geom - c(0, 0)) * scale_factor + c(0, 0)
    out$geometry <- sf::st_as_binary(geom, EWKB = TRUE)
  }

  attr(out, "minx") <- minx
  attr(out, "miny") <- miny
  attr(out, "maxx") <- maxx
  attr(out, "maxy") <- maxy
  attr(out, "scale_factor") <- scale_factor
  out
}
