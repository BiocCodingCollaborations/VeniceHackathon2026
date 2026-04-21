# segmentation-analysis

## Day 1

### Microbenchmark of segmentation file formats in R 

- Starting from one segmentation mask in three different file formats (json/geojson, h5ad, parquet)
- Measure time and memory usage of a simple task: read the file, subset to a small region, and plot

Prelminary steps:

- Create parquet file from geojson using duckdb
- Explore existing approaches:
    - https://github.com/waldronlab/HistoImagePlot
    - https://cran.r-project.org/web/packages/geojsonsf/index.html

What we have learned so far:

- The h5ad files do not contain true polygons, only bounding boxes and centroids.
- The geojson files contain the polygons for each segmented nucleus.
- The json files contain the polygons + the bounding box for each segmented nucleus + the centroid + metadata.
- The HistoImagePlot package only plots centroids does not attempt to plot polygons.
- The duckspatial package allows for spatial queries without loading polygons in memory (supports parquet and geojson).
- We have a python script to convert json to geojson and from geojson to parquet

We now have the same segmentation in json, geojson, and parquet.

Next steps:

- Select a region of interest.
- Subset the segmentation accordingly.
- Test the performance of the three input files.
- Compare that to: read all in memory and using sf to subset.
- Plot

## Day 2

## Cropping image

- We successfully read a huge image in both OME-TIFF and Zarr format, without loading it in memory.
- We are able to crop a small region and plot the cropped image.



