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
- We are able to subset a geoparquet file with polygons, subset it out of memory and plot an overlay of the polygons and the image.
- We learned that duckspatial can work with both GEOJSON and geoparquet but parquet is about 50x faster to pick 500 polygons from 300k.
- We learned that we can use terra to easily work with huge images with lazy loading. This can be extended to working with zarr files with ZarrArray.
- We have a prototype R function to compute simple statistics (area, mean intensity, etc), but it needs a mask (from the polygons).

Next steps:

- Apply the function on the small subset, rasterizing the polygons.
- Apply this to the whole image using chunks.
- For each chunk, read in the polygons, rasterize them, compute the stats.
- Think about the "optimal" chunk geometry.
- Think about polygons that are only partially in the window.
- Ask Gemini to benchmark terra vs ImageArray
