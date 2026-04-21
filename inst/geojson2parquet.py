from pathlib import Path
import json

import geopandas as gpd

for geojson_file in Path("data").glob("*.geojson"):
    with open(geojson_file, encoding="utf-8") as f:
        features = json.load(f)

    gdf = gpd.GeoDataFrame.from_features(features)
    gdf.to_parquet(geojson_file.with_suffix(".parquet"))

    print("Finished:", geojson_file.with_suffix(".parquet").name)
