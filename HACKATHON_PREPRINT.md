# Scalable Spatial Analysis of Single-Cell Segmentation in R

**GitHub Repository:** [https://github.com/drisso/segmentation-analysis](https://github.com/drisso/segmentation-analysis)

## Motivation

The field of spatial omics and digital pathology has transitioned from low-plex imaging to gigapixel-scale, whole-slide datasets. These datasets often comprise multi-gigabyte whole-slide images (WSIs) paired with segmentation masks that identify hundreds of thousands to millions of individual nuclei or cell boundaries.

Here, we focus on classical **pathology (e.g., H&E)** images, with an example from **The Cancer Image Archive (TCIA)** [10.7937/K9/TCIA.2013.V7FRLZ1D], but the proposed methodology will be applicable to spatial transcriptomics data, when H&E or immunofluorescence images are available in addition to the transcriptomic profiles. This is the case for most datasets.

While the Python ecosystem has established powerful frameworks for image processing (e.g., **SpatialData** [10.1038/s41592-024-02212-x]), the R/Bioconductor ecosystem possesses a uniquely mature and sophisticated suite of **geostatistical and geographical analysis tools** (e.g., **terra** [10.32614/CRAN.package.terra], **sf** [10.32614/RJ-2018-009], and **spatstat** [10.18637/jss.v012.i06]). With the increase in image sizes and number of cells assayed, a major hurdle for R users is the "Raster-Polygon Bottleneck": the inability to efficiently extract image features from massive rasters and intersect them with millions of polygons without exceeding system memory.

The core problem we address is enabling **scalable image feature extraction** in R. By bridging the gap between cloud-optimized image formats (**Zarr**) [https://zarr.dev/] and high-performance spatial databases (**GeoParquet**) [https://geoparquet.org/], we aim to unlock R’s specialized spatial statistics for the digital pathology community.

## Methods

During the hackathon we explored different strategies to efficiently read and crop large images in OME-TIFF and Zarr and to subset large lists of polygons in R.

We developed a pipeline that prioritizes memory efficiency by never materializing more data than is required for a single processing window. Our solution leverages **GeoParquet** for polygon storage and **OME-Zarr** for image storage.

## Example image

We used as an example image, the H&E image with ID `TCGA-02-0001-01Z-00-DX1.83fce43e-42ac-4dcd-b156-2908e75f2e47` from the TCGA, which we retrieve with the `ImageTCGA` Bioconductor package [10.18129/B9.bioc.imageTCGA].

We converted the original svs image into OME-TIFF and OME-Zarr formats. The image has three channels (R, G, B) and 35558 x 48002 pixels. The created Zarr file contains 48 chunks of shape 3 x 6688 x 6688.

In addition to the H&E image, `ImageTCGA` allows to retreive the nuclear segmentation, carried out using **HoverNet** [10.1016/j.media.2019.101563]. Segmentation polygons are available in GEOJSON. Starting from the GEOJSON file, we also created a GeoParquet file with the same polygons. The polygon file contains 333,207 polygons. Note that there is a scale factor of 2 between the coordinates of the polygon file and the image file (as the polygons assume a 40x magnification while the original image for this sample is 20x).

### Technical Stack

Our implementation capitalizes on the efficiency of modern R geographical packages:

- **terra** [10.32614/CRAN.package.terra]: Originally developed for satellite imagery and global-scale geographical data, `terra` provides the high-performance engine required for lazy raster loading and optimized raster-vector intersections in histology images.
- **DuckDB & duckspatial** [10.32614/CRAN.package.duckspatial]: Used for out-of-memory spatial filtering, allowing R to query million-row polygon files as if they were local databases.
- **ZarrArray** [10.18129/B9.bioc.ZarrArray]: Provides the `DelayedArray` [10.18129/B9.bioc.DelayedArray] backend for native Zarr access within the Bioconductor framework.

### Algorithm

Instead of iterating over polygons (the "vector-first" approach), our algorithm iterates over the **intrinsic Zarr chunks** of the image (the "raster-first" approach).

Specifically, for each chunk $C_{i,j}$:

1.  **Spatial Fetch**: A SQL query is pushed to the Parquet file to retrieve only polygons $P$ where $BBox(P) \cap BBox(C_{i,j}) \neq \emptyset$.
2.  **Lazy Read**: The image data for $C_{i,j}$ is read into memory.
3.  **Rasterize**: The selected polygons are rasterized into a label mask.
4.  **Compute**: Pixel statistics (e.g., mean intensity per channel) are computed for each object in the mask.

**Chunk border effects**. The main issue with this approach is what we call a "Border Effect" (Figure 2A).
Our main goal is to avoid reading in memory each chunk more than one time. For this reason, when working in one chunk, we have pixel information only for that chunk even though the retrieved polygons may only partially overlap with the chunk and hence have missing pixel information.

![**Figure 2: Schematic of the Border Effect and Pixel Accumulation Strategy.** (A) The spatial overlap problem where a polygon (cell) spans across the boundary of two Zarr chunks. (B) The two-step accumulation solution: when Chunk A is processed, partial pixel values for the polygon are stored in a memory buffer. When Chunk B is subsequently read, its corresponding pixels are combined with the buffered data to compute the final global statistic (e.g., mean intensity) without data loss.](img/border_effect_schematic.png)

Our proposed solution is to implement a pixel accumulation strategy that combines data from multiple chunks to ensure accurate feature extraction across chunk boundaries without reading each chunk multiple times (Figure 2B). Specifically, when we identify a polygon that only partially overlap the chunk, we store in memory the pixel set of the rasterized shape (the part of the polygon that belongs to the chunk) along with its unique identifier. When reading the chunk(s) that contain the rest of the polygon, we will add to the pixel set until the polygon has been completely rasterized and only then we will compute the statistics for that object.

## Results and discussion

### Random access to polygon files

Our benchmarks demonstrate that pushing spatial queries to the out-of-memory parquet file significantly outperforms traditional GeoJSON-based workflows, enabling the analysis of whole-slide images on standard workstations. Specifically, while `duckspatial` is able to work with both GeoJSON and GeoParquet formats, we found that it is about 50x faster to subset polygons when the input is GeoParquet.

### Cropping and visualization of large images

Our benchmark shows that reading, cropping, and visualizing large OME-TIFF images can be done efficiently using `terra`'s `rast()` and `crop()` functions.
Similarly, for large OME-Zarr images, a combination of `ZarrArray` subsetting and `terra`'s `rast()` function works well.

We successfully visualized the intersection of these high-speed queries with high-resolution pathology crops (Figure 1).

![**Figure 1: Overlay of Segmentation Polygons on H&E Image Crop.** This figure demonstrates the successful intersection of a lazily loaded Zarr image and polygons retrieved via a spatial query from GeoParquet. The crop represents a small region of a TCGA H&E whole-slide image.](img/overlay.png)

### Mask creation and statistics computation

Putting all together, we have developed a prototype function that allowed us to:

- iterate over the chunks of a Zarr image, 
- select the polygons that overlap with each chunk,
- rasterize the polygons into an image mask,
- compute the average pixel intensity per each RGB channel over the segmented objects in the mask.

Using our example image, we are able to run such analyses in $986\pm 21$ seconds on a standard laptop. Note that for the moment we are simply skipping the polygons that overlap more than one chunk, sidestepping the border effect issue. Future work will be dedicated to solving this problem.


## 5. References
- [10.1038/s41592-024-02212-x] Marconato et al., "SpatialData: an open and universal data framework for spatial omics," *Nature Methods*, 2024.
- [10.18637/jss.v012.i06] Baddeley & Turner, "Spatstat: An R Package for Analyzing Point Patterns," *Journal of Statistical Software*, 2005.
- [10.32614/CRAN.package.terra] Hijmans, "terra: Spatial Data Analysis," 2026.
- [10.32614/RJ-2018-009] Pebesma, "Simple Features for R," *The R Journal*, 2018.
- [10.18129/B9.bioc.ZarrArray] Pagès et al., "ZarrArray: Bring Zarr datasets in R as DelayedArray objects," Bioconductor, 2026.
- [10.1016/j.media.2019.101563] Graham et al., "Hover-Net: Simultaneous segmentation and classification of nuclei in multi-tissue histology images," *Medical Image Analysis*, 2019.
