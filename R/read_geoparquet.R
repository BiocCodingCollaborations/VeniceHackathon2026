library(DBI)
library(duckdb)

read_geoparquet <- function(path, minx = -1e308, miny = -1e308, maxx = 1e308, maxy = 1e308) {
  con <- DBI::dbConnect(duckdb::duckdb())

  out <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT * ",
      "FROM read_parquet('", path, "') ",
      "WHERE maxx >= ", minx, " ",
      "AND minx <= ", maxx, " ",
      "AND maxy >= ", miny, " ",
      "AND miny <= ", maxy
    )
  )

  DBI::dbDisconnect(con, shutdown = TRUE)
  out
}

iter_geoparquet <- function(path, minx = -1e308, miny = -1e308, maxx = 1e308, maxy = 1e308) {
  con <- DBI::dbConnect(duckdb::duckdb())

  res <- DBI::dbSendQuery(
    con,
    paste0(
      "SELECT * ",
      "FROM read_parquet('", path, "') ",
      "WHERE maxx >= ", minx, " ",
      "AND minx <= ", maxx, " ",
      "AND maxy >= ", miny, " ",
      "AND miny <= ", maxy
    )
  )

  function() {
    out <- DBI::dbFetch(res, n = 1)

    if (nrow(out) == 0) {
      DBI::dbClearResult(res)
      DBI::dbDisconnect(con, shutdown = TRUE)
      return(NULL)
    }

    out
  }
}

# example:
# x <- filter_geoparquet_duckdb(
#   path = "data/nucleus_boundaries.parquet",
#   query_minx = 0,
#   query_miny = 0,
#   query_maxx = 40100,
#   query_maxy = 40150
# )
#
# it <- iter_geoparquet("data/nucleus_boundaries.parquet")
# it()
# it()
