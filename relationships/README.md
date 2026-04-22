# SpatialData Element Relationships — Graph Visualization

**[View the interactive graph (graph_all.html)](graph_all.html)**

---

> **This code is vibe-coded and not ready for production.**
> The purpose is to explore a design and perform quick prototyping before a full implementation in the SpatialData ecosystem.

Based on work from the Padua scverse hackathon: https://github.com/scverse/2026_04_hackathon_padua/issues/6.

---

## What this is

An interactive graph viewer that renders the element relationships and cross-sdata spatial-join suggestions across multiple `SpatialData` objects. Nodes are elements (shapes, labels, images, tables); edges encode join strategies or suggested spatial joins.

## Generating the HTML

```bash
python run_graph_viz.py
```

This writes `graph_all.html`. Open it in any browser.

## Getting the data

### `sdata_qc`

```python
sdata_qc = sd.read_zarr('data/sdata_xenium_crop_with_qc.zarr')
```

Available via the Venice 2026 Zulip channel:
https://community-bioc.zulipchat.com/#narrow/channel/589031-Venice-2026-spatial/topic/.5Btrack.5D.20interoperability/with/590188093

### The other four SpatialData objects

Run the following notebook to generate them:
https://github.com/scverse/spatialdata-notebooks/blob/main/notebooks/paper_reproducibility/00_xenium_and_visium.ipynb

- The three `data_aligned.zarr` datasets (`xenium_rep1_io`, `xenium_rep2_io`, `visium_associated_xenium_io`) are produced as output of the notebook.
- `sandbox.zarr` can be downloaded as described in the notebook.

Update the paths in `run_graph_viz.py` to point to your local copies.
