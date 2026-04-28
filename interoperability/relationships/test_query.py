import warnings; warnings.filterwarnings('ignore')
import spatialdata as sd
from query import check_relationships, query, query_cross

sdata_qc = sd.read_zarr('data/sdata_xenium_crop_with_qc.zarr')
sdata_qc.attrs["element_relationships"] = [[
    {"element": "cell_boundaries",    "type": "dataframe", "join_strategy": "index"},
    {"element": "cell_borders",       "type": "dataframe", "join_strategy": "index"},
    {"element": "cell_centers",       "type": "dataframe", "join_strategy": "index"},
    {"element": "nucleus_boundaries", "type": "dataframe", "join_strategy": "index"},
    {"element": "table",              "type": "dataframe", "join_strategy": "cell_id"},
]]
sdata_qc.attrs["sjoin_suggestions"] = [["transcripts", "cell_boundaries"]]

# ── check_relationships ───────────────────────────────────────────────────────
print("=" * 70)
print("check_relationships(sdata_qc)")
print("=" * 70)
check_relationships(sdata_qc)

# ── depth=1, no IDs ───────────────────────────────────────────────────────────
print("=" * 70)
print("query(sdata_qc, 'cell_boundaries', depth=1)")
print("=" * 70)
result = query(sdata_qc, "cell_boundaries", depth=1)
for name, el in result.items():
    n = el.n_obs if hasattr(el, 'n_obs') else len(el)
    print(f"  {name:<30}  {n:>6} rows")
print()

# ── depth="all" ───────────────────────────────────────────────────────────────
print("=" * 70)
print("query(sdata_qc, 'cell_boundaries', depth='all')")
print("=" * 70)
result = query(sdata_qc, "cell_boundaries", depth="all")
for name, el in result.items():
    n = el.n_obs if hasattr(el, 'n_obs') else len(el)
    print(f"  {name:<30}  {n:>6} rows")
print()

# ── type filter ───────────────────────────────────────────────────────────────
print("=" * 70)
print("query(sdata_qc, 'cell_boundaries', depth='all', types=['shapes','tables'])")
print("=" * 70)
result = query(sdata_qc, "cell_boundaries", depth="all", types=["shapes", "tables"])
for name, el in result.items():
    n = el.n_obs if hasattr(el, 'n_obs') else len(el)
    print(f"  {name:<30}  {n:>6} rows")
print()

# ── ID subsetting ─────────────────────────────────────────────────────────────
# pick 3 real cell IDs from cell_boundaries index
test_ids = list(sdata_qc["cell_boundaries"].index[:3])
print("=" * 70)
print(f"query(sdata_qc, 'cell_boundaries', depth='all', ids={test_ids})")
print("=" * 70)
result = query(sdata_qc, "cell_boundaries", depth="all", ids=test_ids)
for name, el in result.items():
    n = el.n_obs if hasattr(el, 'n_obs') else len(el)
    print(f"  {name:<30}  {n:>6} rows")
print()

# ── query_cross ───────────────────────────────────────────────────────────────
print("=" * 70)
print("query_cross([sdata_qc], root=('sdata_0', 'cell_boundaries'), depth=1)")
print("=" * 70)
result = query_cross([sdata_qc], root=("sdata_0", "cell_boundaries"), depth=1)
for (sname, ename), el in result.items():
    n = el.n_obs if hasattr(el, 'n_obs') else len(el)
    print(f"  ({sname}, {ename:<28})  {n:>6} rows")
print()
