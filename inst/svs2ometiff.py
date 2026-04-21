from pathlib import Path

import numpy as np
import openslide
import tifffile

for svs_file in Path("data").rglob("*.svs"):
    ome_tiff_file = svs_file.with_suffix(".ome.tiff")

    slide = openslide.OpenSlide(str(svs_file))
    width, height = slide.dimensions
    image = slide.read_region((0, 0), 0, (width, height)).convert("RGB")
    image = np.asarray(image)

    tifffile.imwrite(ome_tiff_file, image, ome=True, metadata={"axes": "YXS"})

    print("Finished:", ome_tiff_file.name)
