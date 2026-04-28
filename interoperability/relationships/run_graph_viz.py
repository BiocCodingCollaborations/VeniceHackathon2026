import warnings
warnings.filterwarnings('ignore')

import spatialdata as sd
from graph_viz import show_graph, export_html

sdata_qc  = sd.read_zarr('data/sdata_xenium_crop_with_qc.zarr')
sdata_ann = sd.read_zarr('data/sandbox.zarr')
sdata_xe1 = sd.read_zarr('/Users/macbook/embl/projects/basel/spatialdata-sandbox/xenium_rep1_io/data_aligned.zarr')
sdata_xe2 = sd.read_zarr('/Users/macbook/embl/projects/basel/spatialdata-sandbox/xenium_rep2_io/data_aligned.zarr')
sdata_vis = sd.read_zarr('/Users/macbook/embl/projects/basel/spatialdata-sandbox/visium_associated_xenium_io/data_aligned.zarr')

# ── set relationship attrs (mirrors read_sdatas.py) ───────────────────────────

sdata_qc.attrs["element_relationships"] = [[
    {"element": "cell_boundaries",    "type": "dataframe", "join_strategy": "index"},
    {"element": "cell_borders",       "type": "dataframe", "join_strategy": "index"},
    {"element": "cell_centers",       "type": "dataframe", "join_strategy": "index"},
    {"element": "nucleus_boundaries", "type": "dataframe", "join_strategy": "index"},
    {"element": "table",              "type": "dataframe", "join_strategy": "cell_id"},
]]
sdata_qc.attrs["sjoin_suggestions"] = [
    ["transcripts", "cell_boundaries"],
]

sdata_xe1.attrs["element_relationships"] = [[
    {"element": "cell_boundaries", "type": "dataframe", "join_strategy": "index"},
    {"element": "cell_circles",    "type": "dataframe", "join_strategy": "index"},
    {"element": "table",           "type": "dataframe", "join_strategy": "cell_id"},
]]
sdata_xe1.attrs["sjoin_suggestions"] = [
    ["transcripts", "cell_boundaries"],
]

sdata_xe2.attrs["element_relationships"] = [[
    {"element": "cell_boundaries",    "type": "dataframe", "join_strategy": "index"},
    {"element": "cell_circles",       "type": "dataframe", "join_strategy": "index"},
    {"element": "nucleus_boundaries", "type": "dataframe", "join_strategy": "index"},
    {"element": "cell_labels",        "type": "labels",    "join_strategy": "value"},
    {"element": "nucleus_labels",     "type": "labels",    "join_strategy": "value"},
    {"element": "table",              "type": "dataframe", "join_strategy": "cell_id"},
]]
sdata_xe2.attrs["sjoin_suggestions"] = [
    ["transcripts", "cell_boundaries"],
]

sdata_vis.attrs["element_relationships"] = [[
    {"element": "CytAssist_FFPE_Human_Breast_Cancer", "type": "dataframe", "join_strategy": "index"},
    {"element": "table",                              "type": "dataframe", "join_strategy": "spot_id"},
]]

sdata_ann.attrs["element_relationships"] = [[
    {"element": "visium_lm",  "type": "dataframe", "join_strategy": "index"},
    {"element": "xe_rep1_lm", "type": "dataframe", "join_strategy": "index"},
    {"element": "xe_rep2_lm", "type": "dataframe", "join_strategy": "index"},
]]
sdata_ann.attrs["sjoin_suggestions"] = [
    [{"sdata": "sdata_qc",  "element": "cell_boundaries"},
     {"sdata": "sdata_xe1", "element": "cell_boundaries"}],
    [{"sdata": "sdata_xe1", "element": "cell_boundaries"},
     {"sdata": "sdata_xe2", "element": "cell_boundaries"}],
    [{"sdata": "sdata_ann", "element": "rois"},
     {"sdata": "sdata_xe1", "element": "cell_boundaries"}],
    [{"sdata": "sdata_ann", "element": "rois"},
     {"sdata": "sdata_xe2", "element": "cell_boundaries"}],
    [{"sdata": "sdata_ann", "element": "rois"},
     {"sdata": "sdata_vis", "element": "CytAssist_FFPE_Human_Breast_Cancer"}],
]

# # ── per-sdata HTML (open in any browser) ─────────────────────────────────────
#
# export_html(sdata_qc,  "graph_qc.html",  title="sdata_qc")
# export_html(sdata_xe1, "graph_xe1.html", title="sdata_xe1")
# export_html(sdata_xe2, "graph_xe2.html", title="sdata_xe2")
# export_html(sdata_vis, "graph_vis.html", title="sdata_vis")
# export_html(sdata_ann, "graph_ann.html", title="sdata_ann (landmarks)")

# ── multi-sdata combined view ─────────────────────────────────────────────────

export_html(
    {
        "sdata_qc":  sdata_qc,
        "sdata_xe1": sdata_xe1,
        "sdata_xe2": sdata_xe2,
        "sdata_vis": sdata_vis,
        "sdata_ann": sdata_ann,
    },
    "graph_all.html",
    title="All SpatialData Objects",
    cross_sdata_file="cross_sdata.json",
)

# ── matplotlib fallback ───────────────────────────────────────────────────────
# show_graph(sdata_qc,  title="sdata_qc")
# show_graph(sdata_xe2, title="sdata_xe2")
