library(arrow)
library(dplyr)

parquet_file <- Sys.glob("data/*.parquet")[1]
x <- read_parquet(parquet_file, as_data_frame = FALSE)

print(x$schema)
print(names(x$schema$metadata))
print(x$schema$metadata)

query_minx <- 0
query_miny <- 0
query_maxx <- 40100
query_maxy <- 40150

filtered <- open_dataset(parquet_file, format = "parquet") |>
  filter(
    maxx >= query_minx,
    minx <= query_maxx,
    maxy >= query_miny,
    miny <= query_maxy
  ) |>
  collect()

print(nrow(filtered))
print(head(filtered))
