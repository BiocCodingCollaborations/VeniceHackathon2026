# VeniceInterop

This repository was built during the 2026 Hackathon in San Servolo, Venice.

It shows a vignette how to:
- read a `SpatialData` object in `python`
- write the `SpatialData` object in `python`
- read the `SpatialData` object in `R`
- run `R` methods on the `SpatialData` object (`spacexr`, `sosta`)
- plot the updated `SpatialData` object in `R` using `SpatialData.plot`

In the process of building this vignette we updated several key components of both the `SpatialData` and `SpatialData.plot` packages: 
- lazy queries of `SpatialData` objects such as a bounding-box or polygons with `duckspatial`
- updating `SpatialData.plot` to work on `geom_sf` in response to the `SpatialData` changes. 
- enabling plotting of multichannel images in `SpatialData.plot`

[click me](http://htmlpreview.github.io/?https://github.com/HelenaLC/VeniceInterop/blob/main/Interoperability.html)
