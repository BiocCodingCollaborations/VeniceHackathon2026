##
import spatialdata as sd

sdata_qc = sd.read_zarr('data/sdata_xenium_crop_with_qc.zarr')
sdata_ann = sd.read_zarr('data/sandbox.zarr')

# these are created by this notebook: https://github.com/scverse/spatialdata-notebooks/blob/main/notebooks/paper_reproducibility/00_xenium_and_visium.ipynb
sdata_xe1 = sd.read_zarr('/Users/macbook/embl/projects/basel/spatialdata-sandbox/xenium_rep1_io/data_aligned.zarr')
sdata_xe2 = sd.read_zarr('/Users/macbook/embl/projects/basel/spatialdata-sandbox/xenium_rep2_io/data_aligned.zarr')
sdata_vis = sd.read_zarr('/Users/macbook/embl/projects/basel/spatialdata-sandbox/visium_associated_xenium_io/data_aligned.zarr')

##
print(f"\nsdata_qc:\n{sdata_qc}")
print(f"\nsdata_xe1:\n{sdata_xe1}")
print(f"\nsdata_xe2:\n{sdata_xe2}")
print(f"\nsdata_vis:\n{sdata_vis}")
print(f"\nsdata_ann:\n{sdata_ann}")

##
# this follows more or less the example from here https://github.com/scverse/2026_04_hackathon_padua/issues/6#issuecomment-4251771224
# but we need to make some changes
# this is what we need:
# in sdata.attrs, we need to have a list of lists of dicts (I'll explain later how these dicts are). i.e. Each element fo the list is a list of related elements that share the same indexing system.
# example 1
# [[{element: cells0, type: dataframe, join_strategy: index}, {element: nuclei0, type: dataframe, join_strategy: index}, {element: labels0, type: labels, join_startegy: value}], [{element: cells0, type: dataframe join_stategy: "cellpose_ids"}, {element: cells1, type: dataframe, join_strategy: "index"]]
# - sjoin will not be encoded in the graph. Istead the user will be able to sjoin different elements and then encode the overlap as described above
# - a sjoin operation will be made available via an API that will check for matching elements and save these as an additional column in one of the two elements
# - there should be a way to build the graph that describes the relationships between the various entities since not all the pair-wise combinations may be listed, but there could be relationships that could be inferred transitively.
# - the type of link sohuld be:
#   - dataframe
#   - xarray (when the link is given in terms of a coordinate), for instance one entity for each channel of a dataframe
#   - element name, for instance a table referring to images on disk
# - there should be an API that in a very lightweight way checks if the relationships are 1-to-1, 1-to-many, and if there is a perfect match, and not some entities not mapped to other entities. This function would consider the transitive nature of the graph. Also it should check if, when applicable, the order in which the instances (e.g. rows) appear in an element (e.g. dataframe) is idenatical.
# all the links
# --- sdata_qc ---
# transcripts: could be potentially mapped via sjoin into cell_borders, cell_boundaries, cell_centers, nucleus_boundaries. We could show an exapmle of this via APIs
# cell_borders, cell_boundaries, cell_centers, nucleus_boundaries: all mapped together via cell_id index
#   NOTE: cell_boundaries has 1 extra cell (id=77140) not present in cell_borders, cell_centers, or table
#   NOTE: nucleus_boundaries is a subset (1501 out of 1562 cells have a nucleus boundary)
# table: maps to cells via obs["cell_id"] column; 1562 rows match the 1562-cell subset (not the extra cell in cell_boundaries)
# extra elements that are not there but we need to add:
#   centroids. We would have to precompute centroids. Better to have them directly in the anndata object a an obs column.

# --- sdata_xe1 ---
# cell_boundaries (167780) and cell_circles (167780) share identical cell_id index
# table: maps to cell_circles via obs["cell_id"]; perfect 1-to-1, same order

# --- sdata_xe2 ---
# cell_boundaries, cell_circles, nucleus_boundaries: all 118752 cells with identical cell_id indices (1..118752)
#   here nucleus_boundaries covers ALL cells (unlike sdata_qc where it was a subset)
# cell_labels: label image where pixel value = cell_id (join by value)
# nucleus_labels: label image where pixel value = cell_id (join by value)
# table: maps to cell_circles via obs["cell_id"]; perfect 1-to-1

# --- sdata_vis ---
# CytAssist_FFPE_Human_Breast_Cancer shapes (4992 spots) and table (4992 rows) share spot_id
# table: maps to CytAssist_FFPE_Human_Breast_Cancer via obs["spot_id"]; perfect 1-to-1

# --- sdata_ann ---
# visium_lm, xe_rep1_lm, xe_rep2_lm: 3 corresponding landmarks (index 0,1,2) linking across datasets;
#   same index, same order, 1-to-1 — row i in each is the same anatomical landmark
# box, rois: standalone spatial annotation polygons, no indexing relationship to other elements

##
# relationship schemas to store in sdata.attrs["element_relationships"]
# each inner list = a group of elements sharing the same instance indexing system
# dict keys: element (name), type (dataframe|labels|xarray), join_strategy (index|value|<column_name>)

rel_qc = [
    [
        {"element": "cell_boundaries",    "type": "dataframe", "join_strategy": "index"},
        {"element": "cell_borders",       "type": "dataframe", "join_strategy": "index"},
        {"element": "cell_centers",       "type": "dataframe", "join_strategy": "index"},
        {"element": "nucleus_boundaries", "type": "dataframe", "join_strategy": "index"},
        {"element": "table",              "type": "dataframe", "join_strategy": "cell_id"},
    ]
    # transcripts not included: linked via sjoin, not a shared indexing system
]

rel_xe1 = [
    [
        {"element": "cell_boundaries", "type": "dataframe", "join_strategy": "index"},
        {"element": "cell_circles",    "type": "dataframe", "join_strategy": "index"},
        {"element": "table",           "type": "dataframe", "join_strategy": "cell_id"},
    ]
]

rel_xe2 = [
    [
        {"element": "cell_boundaries",    "type": "dataframe", "join_strategy": "index"},
        {"element": "cell_circles",       "type": "dataframe", "join_strategy": "index"},
        {"element": "nucleus_boundaries", "type": "dataframe", "join_strategy": "index"},
        {"element": "cell_labels",        "type": "labels",    "join_strategy": "value"},
        {"element": "nucleus_labels",     "type": "labels",    "join_strategy": "value"},
        {"element": "table",              "type": "dataframe", "join_strategy": "cell_id"},
    ]
]

rel_vis = [
    [
        {"element": "CytAssist_FFPE_Human_Breast_Cancer", "type": "dataframe", "join_strategy": "index"},
        {"element": "table",                              "type": "dataframe", "join_strategy": "spot_id"},
    ]
]

rel_ann = [
    # corresponding landmarks across the 3 datasets; same index order, 1-to-1
    [
        {"element": "visium_lm",  "type": "dataframe", "join_strategy": "index"},
        {"element": "xe_rep1_lm", "type": "dataframe", "join_strategy": "index"},
        {"element": "xe_rep2_lm", "type": "dataframe", "join_strategy": "index"},
    ]
    # box and rois are standalone annotations, not linked
]

sdata_qc.attrs["element_relationships"] = rel_qc
sdata_xe1.attrs["element_relationships"] = rel_xe1
sdata_xe2.attrs["element_relationships"] = rel_xe2
sdata_vis.attrs["element_relationships"] = rel_vis
sdata_ann.attrs["element_relationships"] = rel_ann

##
# sjoin_suggestions: potential spatial joins not yet computed.
# format: a list of 2-element lists — [element_a, element_b] — symmetric/undirected.
# cross-sdata elements are identified as {"sdata": <name>, "element": <element_name>}.
# once a sjoin is computed, its result is stored as a column on one of the elements,
# and the suggestion is replaced by a real edge in element_relationships.
# cross-sdata suggestions are stored in sdata_ann as it is the coordination/registration sdata.

# within sdata_qc: transcripts <-> cell shapes
sjoin_qc = [
    ["transcripts", "cell_boundaries"],
]

# within sdata_xe1
sjoin_xe1 = [
    ["transcripts", "cell_boundaries"],
]

# within sdata_xe2
sjoin_xe2 = [
    ["transcripts", "cell_boundaries"],
]

# cross-sdata suggestions stored in sdata_ann
# sdata names match variable names in this script; sdata_vis uses spot shapes
sjoin_ann_cross = [
    # cell overlap between replicate xenium datasets and between xe1 crop and xe2
    [{"sdata": "sdata_qc",  "element": "cell_boundaries"},
     {"sdata": "sdata_xe1", "element": "cell_boundaries"}],
    [{"sdata": "sdata_xe1", "element": "cell_boundaries"},
     {"sdata": "sdata_xe2", "element": "cell_boundaries"}],
    # rois (annotation polygons) sjoin'd into cell shapes in each dataset
    [{"sdata": "sdata_ann", "element": "rois"},
     {"sdata": "sdata_xe1", "element": "cell_boundaries"}],
    [{"sdata": "sdata_ann", "element": "rois"},
     {"sdata": "sdata_xe2", "element": "cell_boundaries"}],
    [{"sdata": "sdata_ann", "element": "rois"},
     {"sdata": "sdata_vis", "element": "CytAssist_FFPE_Human_Breast_Cancer"}],
]

sdata_qc.attrs["sjoin_suggestions"]  = sjoin_qc
sdata_xe1.attrs["sjoin_suggestions"] = sjoin_xe1
sdata_xe2.attrs["sjoin_suggestions"] = sjoin_xe2
sdata_ann.attrs["sjoin_suggestions"] = sjoin_ann_cross

print("sdata_qc sjoin_suggestions:  ", sdata_qc.attrs["sjoin_suggestions"])
print("sdata_ann sjoin_suggestions: ", sdata_ann.attrs["sjoin_suggestions"])

## ergonomic simulation