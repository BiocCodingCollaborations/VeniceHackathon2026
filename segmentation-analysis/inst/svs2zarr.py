from pathlib import Path

import numpy as np
import openslide
from spatialdata import SpatialData
from spatialdata.models import Image2DModel

for svs_file in Path("data").rglob("*.svs"):
    zarr_file = svs_file.with_suffix(".zarr")

    slide = openslide.OpenSlide(str(svs_file))
    width, height = slide.dimensions
    image = slide.read_region((0, 0), 0, (width, height)).convert("RGB")
    image = np.asarray(image)
    image = np.transpose(image, (2, 0, 1))

    image = Image2DModel.parse(image, dims=("c", "y", "x"))
    sdata = SpatialData(images={"images": image})
    sdata.write(zarr_file)

    print("Finished:", zarr_file.name)
