import json
from pathlib import Path

import geojson

scale_factor = 2  # for TCGA-02-0001-01Z-00-DX1.83fce43e-42ac-4dcd-b156-2908e75f2e47

for json_file in Path("data").glob("*.json"):
    output_file = json_file.with_suffix(".geojson")
    DATA = json.load(open(json_file, "r"))

    GEOdata = []
    cells = list(DATA["nuc"].keys())

    for cell in cells:
        nuc = DATA["nuc"][cell]
        dict_data = {}
        cc = nuc["contour"]

        cc = [[x / scale_factor, y / scale_factor] for x, y in cc]
        cc.append(cc[0])

        bbox = [[x / scale_factor, y / scale_factor] for x, y in nuc["bbox"]]
        centroid = [coord / scale_factor for coord in nuc["centroid"]]
        cell_type_id = nuc["type"]

        dict_data["type"] = "Feature"
        dict_data["id"] = cell
        dict_data["geometry"] = {"type": "Polygon", "coordinates": [cc]}
        dict_data["properties"] = {
            "bbox": bbox,
            "centroid": centroid,
            "type_id": cell_type_id,
            "type_prob": nuc["type_prob"],
            "magnification": DATA["mag"],
            "scale_factor": scale_factor,
        }

        GEOdata.append(dict_data)

    with open(output_file, "w") as outfile:
        geojson.dump(GEOdata, outfile)

    print("Finished:", output_file.name)
