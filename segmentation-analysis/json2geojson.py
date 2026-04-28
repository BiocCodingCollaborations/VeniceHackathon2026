import json
import geojson
import pandas as pd
from pathlib import Path
import openslide  


type_info_path = Path("/home/ibillato/hovernet/output/type_info.json")
json_folder_path = Path("/home/ibillato/hovernet/output/json/")
output_path = Path("/home/ibillato/hovernet/output/geojson/")
magnification_info_path = Path("/home/ibillato/qupath/image_magnification_TCGA.xlsx")

info = json.load(open(type_info_path, 'r'))



magnification_info = pd.read_excel(magnification_info_path).set_index("Image Name")


for json_file in json_folder_path.glob("*.json"):
    new_fn = json_file.stem + '.geojson'
    if (output_path / new_fn).exists():
        print(f'Skipping {new_fn} (already exists)')
        continue

    DATA = json.load(open(json_file, 'r'))

    svs_file = json_file.stem + ".svs"
    magnification = magnification_info.loc[svs_file, "Magnification"] if svs_file in magnification_info.index else "Unknown"

   
    scale_factor = 2 if magnification == "20" else 1

    GEOdata = []
    cells = list(DATA['nuc'].keys())

    for cell in cells:
        dict_data = {}
        cc = DATA['nuc'][cell]['contour']
        
        cc = [[x / scale_factor, y / scale_factor] for x, y in cc]
        cc.append(cc[0])  # chiude il poligono

        cell_type = info[str(DATA['nuc'][cell]['type'])][0]

        dict_data["type"] = "Feature"
        dict_data["id"] = cell
        dict_data["geometry"] = {"type": "Polygon", "coordinates": [cc]}
        dict_data["properties"] = {
            "isLocked": "false",
            "measurements": [],
            "classification": {"name": cell_type, "color": [255, 0, 0]}
        }

        GEOdata.append(dict_data)

    with open(output_path.joinpath(new_fn), 'w') as outfile:
        geojson.dump(GEOdata, outfile)

    print('Finished:', new_fn)
