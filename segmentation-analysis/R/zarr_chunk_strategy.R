library(Rarr)
library(sf)
library(arrow)   # read parquet
library(terra)   # rasterize polygons within chunk
library(dplyr)
library(duckspatial)

sf_use_s2(FALSE)

# =============================================================================
# Strategy 3: Chunk-Aligned Batching
#
# Key idea: iterate over Zarr CHUNKS (not polygons). Each chunk is read once,
# and all polygons overlapping that chunk are processed against it.
# Polygons spanning multiple chunks are accumulated across chunk passes.
# =============================================================================

poly_crs <- 4326   # CRS for polygon coordinates (micron space, no units)
# --- 1. Read polygons from parquet -----------------------------------------

# Scale factor from the shapes zarr.json coordinateTransformations.
# In SpatialData, shapes are stored in micron space. The scale factor converts
# microns -> pixels (i.e. pixel_coord = micron_coord * scale).
# Read directly from zarr.json: .zattrs of the shapes element, "coordinateTransformations"
# e.g. jsonlite::fromJSON("…/shapes.zarr.json")$coordinateTransformations[[1]]$scale
shapes_to_pixel_scale <- 4.705882352941177   # pixels per micron (1 / 0.2125 µm)

# polys stays as a lazy duckspatial table — never fully materialised
polys <- duckspatial::ddbs_open_dataset(
    "~/projects/venice-hackathon/sdata_xenium_crop.zarr/shapes/nucleus_boundaries/shapes.parquet",
    crs = poly_crs
)

# Identify the ID / name column to carry through to results
# e.g. polys$id, polys$classification  — pick whichever is the cell identifier
id_col <- "cell_id"

# Apply coordinate transform: scale geometry from microns to image pixel coordinates.
# ddbs_scale multiplies x and y by the given factors before any spatial query,
# so all subsequent chunk bboxes (which are in pixel units) will align correctly.
polys <- duckspatial::ddbs_scale(polys, x_scale = shapes_to_pixel_scale, y_scale = shapes_to_pixel_scale)


# --- 2. Open Zarr and inspect chunk layout ----------------------------------

zarr_path <- "~/projects/venice-hackathon/sdata_xenium_crop.zarr/images/morphology_focus/0"                # path to .zarr store (or OME-Zarr group)

# Read the .zarray metadata for the array of interest
# For OME-Zarr the path is typically "<zarr_path>/0" for the full-resolution level
zarray_info <- Rarr::zarr_overview(zarr_path, as_data_frame = TRUE)

# Extract image dimensions and chunk shape.
# For a 2D grayscale array the shape is [Y, X]; for multichannel [C, Y, X].
# Rarr stores this in the metadata; parse it from the .zarray JSON if needed:
img_shape   <- zarray_info$dim[[1]]      # e.g. [33776, 46030] or [3, 33776, 46030]
chunk_shape <- zarray_info$chunk_dim[[1]]     # e.g. [1024, 1024]   or [1, 1024, 1024]

# Determine which axes are spatial (Y and X). Adjust indices if multichannel.
# Assumption: last two axes are Y (rows) and X (cols).
n_dims  <- length(img_shape)
y_ax    <- n_dims - 1   # 1-based index for Rarr slice (n_dims - 1 + 1 = n_dims... see below)
x_ax    <- n_dims

img_h   <- img_shape[n_dims - 1]    # total height in pixels
img_w   <- img_shape[n_dims]        # total width  in pixels
chunk_h <- chunk_shape[n_dims - 1]
chunk_w <- chunk_shape[n_dims]


# --- 3. Build chunk grid ----------------------------------------------------

# Grid of chunk origins (1-based R indices, top-left corner of each chunk)
row_origins <- seq(1, img_h, by = chunk_h)
col_origins <- seq(1, img_w, by = chunk_w)

chunk_grid <- expand.grid(row0 = row_origins, col0 = col_origins)
#   row0 / col0 : 1-based R index of chunk top-left pixel


# --- 4. Initialise per-polygon accumulators ---------------------------------
#
# We compute running statistics so we never need to store all pixel values.
# For each polygon track: pixel count, sum, and sum-of-squares (for variance).
# Add more accumulators for other stats as needed.

# Hash environment accumulator — grows dynamically as polygons are first encountered;
# keys are polygon IDs (coerced to character). No upfront knowledge of all IDs needed.
stats_acc <- new.env(hash = TRUE, parent = emptyenv())

# Metadata (non-geometry attributes) collected incrementally per chunk.
# Avoids a global ddbs_collect() on the full polygon dataset.
poly_meta <- list()


# --- 5. Chunk loop ----------------------------------------------------------

for (i in seq_len(nrow(chunk_grid))) {

    row0 <- chunk_grid$row0[i]
    col0 <- chunk_grid$col0[i]

    # Pixel bounding box of this chunk (1-based R indices, inclusive)
    row1 <- min(row0 + chunk_h - 1, img_h)
    col1 <- min(col0 + chunk_w - 1, img_w)

    # Convert pixel bbox to an sf polygon in the same coordinate system as polys.
    # Pixel i (1-based) occupies the spatial interval [i-1, i], so subtract 1
    # from the lower bound to get 0-based spatial coordinates matching the data.
    chunk_bbox <- st_bbox(
        c(xmin = col0 - 1, ymin = row0 - 1, xmax = col1, ymax = row1),
        crs = st_crs(poly_crs)
    )
    chunk_sf <- st_sf(geom = st_as_sfc(chunk_bbox))

    # --- 5a. Spatial query: push bbox filter down into DuckDB/parquet ----------
    # ddbs_filter translates to a spatial SQL predicate executed against the
    # parquet file — only matching rows are transferred from disk to memory.
    chunk_polys <- ddbs_filter(polys, chunk_sf) |> ddbs_collect()

    if (nrow(chunk_polys) == 0) next   # no polygons in this chunk — skip I/O

    # Collect attributes for polygons we are seeing for the first time
    new_ids <- setdiff(as.character(chunk_polys[[id_col]]), names(poly_meta))
    if (length(new_ids) > 0) {
        new_rows <- chunk_polys[as.character(chunk_polys[[id_col]]) %in% new_ids, ] |>
            st_drop_geometry() |>
            select(all_of(id_col))   # extend with other attribute columns as needed
        poly_meta[new_ids] <- split(new_rows, seq_len(nrow(new_rows)))
    }

    # --- 5b. Read image data for this chunk from Zarr ------------------------
    # Rarr uses 1-based indices and inclusive ranges.
    # For a 2D [Y, X] array:
    chunk_data <- read_zarr_array(
        zarr_path,
        index = list(
            1,
            row0:row1,
            col0:col1
        )
    )
    # chunk_data is a matrix of dimensions [chunk_rows, chunk_cols].
    # For multichannel [C, Y, X], add a channel index or loop over channels.

    # --- 5c. For each overlapping polygon, extract pixel values directly ------

    # Load chunk image data into a SpatRaster with correct spatial extent.
    # terra::rast(matrix) sets extent to [0,ncols] x [0,nrows] by default;
    # we override it to match the pixel coordinate space used by the polygons.
    chunk_rast <- terra::rast(chunk_data)
    terra::ext(chunk_rast) <- terra::ext(col0 - 1, col1, row0 - 1, row1)

    for (j in seq_len(nrow(chunk_polys))) {

        poly_id   <- chunk_polys[[id_col]][j]
        poly_geom <- chunk_polys[j, ]

        # Clip polygon to chunk extent (handles polygons that hang over the edge)
        clipped <- tryCatch(
            st_intersection(poly_geom, chunk_sf),
            error = function(e) NULL
        )
        if (is.null(clipped) || nrow(clipped) == 0) next

        # Extract pixel values under the polygon using terra::extract.
        # No rasterization or mask matrix needed — terra handles the
        # geometry-raster intersection internally.
        # st_set_crs(NA) ensures vector and raster CRS both NA (no reprojection).
        poly_sv    <- terra::vect(st_set_crs(clipped, NA))
        extracted  <- terra::extract(chunk_rast, poly_sv, ID = FALSE)
        pixel_vals <- extracted[[1]]
        pixel_vals <- pixel_vals[!is.na(pixel_vals)]

        if (length(pixel_vals) == 0) next

        # Accumulate into running stats (hash environment, keyed by polygon ID)
        key <- as.character(poly_id)
        if (exists(key, envir = stats_acc, inherits = FALSE)) {
            acc <- stats_acc[[key]]
            stats_acc[[key]] <- list(
                n_pixels = acc$n_pixels + length(pixel_vals),
                sum_val  = acc$sum_val  + sum(pixel_vals),
                sum_sq   = acc$sum_sq   + sum(pixel_vals^2)
            )
        } else {
            stats_acc[[key]] <- list(
                n_pixels = length(pixel_vals),
                sum_val  = sum(pixel_vals),
                sum_sq   = sum(pixel_vals^2)
            )
        }
    }
}


# --- 6. Finalise per-polygon summary statistics ----------------------------

# Convert environment to a data frame, then compute summary statistics
results <- do.call(rbind, lapply(ls(stats_acc), function(k) {
    acc <- stats_acc[[k]]
    data.frame(id = k, n_pixels = acc$n_pixels,
               sum_val = acc$sum_val, sum_sq = acc$sum_sq,
               stringsAsFactors = FALSE)
})) |>
    mutate(
        mean_intensity = sum_val / n_pixels,
        variance       = (sum_sq / n_pixels) - mean_intensity^2,
        sd_intensity   = sqrt(pmax(variance, 0))
    ) |>
    select(id, n_pixels, mean_intensity, sd_intensity)

# Join with polygon attributes collected incrementally during the chunk loop
poly_attrs <- do.call(rbind, poly_meta)   # one row per unique polygon

results <- poly_attrs |>
    left_join(results, by = setNames("id", id_col))

print(results)


# =============================================================================
# Notes on extending this outline
#
# Multichannel images:
#   Read chunk_data as [C, Y, X]; loop over channels inside the polygon loop
#   and accumulate per-channel stats (prefix column names with channel index).
#
# Pyramid / resolution levels (OME-Zarr):
#   Use the full-resolution level (".../0") for pixel-accurate stats, or a
#   downsampled level for speed during exploration. Adjust img_shape / chunk_shape.
#
# Parallelism:
#   The chunk loop is embarrassingly parallel. Wrap with parallel::mclapply()
#   or future.apply::future_lapply() over chunk_grid rows, then combine
#   accumulators with Reduce("+", ...) on the stats matrices.
#
# Memory:
#   Only one chunk image (chunk_h * chunk_w * bytes_per_pixel) is in memory at
#   a time. For a 1024x1024 uint16 chunk that is ~2 MB — negligible.
# =============================================================================
