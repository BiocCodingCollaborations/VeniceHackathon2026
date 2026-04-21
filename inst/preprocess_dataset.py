from pathlib import Path
import runpy

inst_dir = Path(__file__).resolve().parent

runpy.run_path(inst_dir / "json2geojson.py")
runpy.run_path(inst_dir / "geojson2parquetbbox.py")
runpy.run_path(inst_dir / "svs2ometiff.py")
