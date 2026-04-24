# Session TODOs

## 1 — Query API for elements and transitively linked data ✓ DONE (query.py)

Design a lightweight Python API so users can retrieve an element and
automatically pull in all related elements (via `element_relationships`)
following transitive links in the graph.

**Sketch of what it should look like:**

```python
# fetch a single element — no traversal
sdata.get("cell_boundaries")

# fetch element + all directly related elements (one hop)
query(sdata, "cell_boundaries", depth=1)
# → returns {
#     "cell_boundaries": GeoDataFrame,
#     "cell_borders":    GeoDataFrame,
#     "cell_centers":    GeoDataFrame,
#     "nucleus_boundaries": GeoDataFrame,
#     "table":           AnnData,
#   }

# with transitive closure (full reachable subgraph)
query(sdata, "cell_boundaries", depth="all")

# filter by element type
query(sdata, "cell_boundaries", depth="all", types=["Shapes", "Tables"])

# subset by instance IDs — pulls matching rows from all linked elements
query(sdata, "cell_boundaries", ids=[6327, 6328, 6330], depth="all")
# → each returned element is already subset to the matching rows

# cross-sdata query (via sjoin_suggestions stored in sdata_ann)
# after a sjoin has been computed and the suggestion promoted to a real edge:
query_cross([sdata_xe1, sdata_xe2], root=("sdata_xe1", "cell_boundaries"), depth=1)
```

**Internal logic:**
- Build an in-memory graph from `element_relationships` at query time
- Walk reachable nodes BFS/DFS up to `depth`
- For each reached element, apply the correct join key (index / column name / value)
  to subset to the requested IDs
- Transitive ID propagation: subset A → find matching IDs in B via join strategy → subset B

**Checks to run first (lightweight, no data load):**
```python
check_relationships(sdata)
# prints per-edge: cardinality (1:1 / 1:N), coverage %, order match, missing IDs
```

---

## 2 — Multi-sample table annotation problem + example

A single column in an AnnData table can only annotate a single sample (sdata).
To reuse the same table across multiple samples, you'd need separate columns per
sample — resulting in a wide table full of NaN values for rows that don't belong
to that sample.

**Show a concrete example of this:**
- Construct a table that annotates two samples simultaneously
- Show the resulting sparse/NaN-heavy column layout
- Discuss or prototype a better representation (e.g. long format, per-sample
  sub-tables, or a multi-index that encodes (sdata, instance_id) pairs)

---

