"""
query.py — element-relationship query API for SpatialData.

Public API
----------
check_relationships(sdata)
    Print per-edge statistics: cardinality, coverage %, order match, missing IDs.

query(sdata, element, depth=1, types=None, ids=None)
    Return {name: element_data} for the root and all reachable elements
    within `depth` hops of the relationship graph.
    If `ids` is given, each element is subset to the matching instances
    (IDs are propagated transitively through the graph).

query_cross(sdatas, root, depth=1, types=None, ids=None)
    Like query() but spanning multiple SpatialData objects.
    sdatas: list or dict {name: SpatialData}.
    root:   (sdata_name, element_name).
    Requires that cross-sdata sjoin suggestions have been promoted to real
    element_relationships entries (i.e. sjoin was computed and result stored).
"""

from __future__ import annotations

from collections import deque
from typing import Union, List, Optional


# ── element-type helpers ──────────────────────────────────────────────────────

def _etype(sdata, name: str) -> str:
    for attr in ("shapes", "labels", "points", "tables", "images"):
        if name in getattr(sdata, attr, {}):
            return attr
    return "unknown"


# ── relationship graph ────────────────────────────────────────────────────────

def _rel_graph(sdata) -> dict:
    """Build full undirected adjacency dict from element_relationships.

    Returns {element: [(neighbor, my_strategy, nbr_strategy), ...]}
    All pairs within each group are connected (not just hub-and-spoke).
    """
    adj: dict[str, list] = {}
    for group in sdata.attrs.get("element_relationships", []):
        for d in group:
            adj.setdefault(d["element"], [])
        for di in group:
            for dj in group:
                if di["element"] == dj["element"]:
                    continue
                adj[di["element"]].append((
                    dj["element"],
                    di["join_strategy"],
                    dj["join_strategy"],
                ))
    return adj


def _strategy_for(sdata, element: str) -> str:
    for group in sdata.attrs.get("element_relationships", []):
        for d in group:
            if d["element"] == element:
                return d["join_strategy"]
    return "index"


# ── ID extraction ─────────────────────────────────────────────────────────────

def _get_join_ids(el, strategy: str, etype: str) -> set:
    """Extract the set of join-key values from an element."""
    try:
        if etype == "tables":
            if strategy == "index":
                return set(el.obs.index.tolist())
            if strategy in el.obs.columns:
                return set(el.obs[strategy].tolist())
        elif etype in ("shapes", "points"):
            if strategy == "index":
                return set(el.index.tolist())
            if strategy in el.columns:
                return set(el[strategy].tolist())
        elif etype == "labels":
            import numpy as np
            vals = np.unique(el.values)
            return {int(v) for v in vals if v != 0}
    except Exception:
        pass
    return set()


# ── subsetting ────────────────────────────────────────────────────────────────

def _subset(el, strategy: str, ids, etype: str):
    """Filter element to rows/pixels where join key ∈ ids.

    Returns the element unchanged if ids is None.
    """
    if ids is None:
        return el
    ids = set(ids)
    try:
        if etype == "tables":
            if strategy == "index":
                return el[el.obs.index.isin(ids)]
            if strategy in el.obs.columns:
                return el[el.obs[strategy].isin(ids)]
        elif etype in ("shapes", "points"):
            if strategy == "index":
                return el[el.index.isin(ids)]
            if strategy in el.columns:
                return el[el[strategy].isin(ids)]
        elif etype == "labels":
            import numpy as np, xarray as xr
            arr = el.values.copy().astype(el.values.dtype)
            arr[~np.isin(arr, list(ids))] = 0
            return xr.DataArray(arr, coords=el.coords, dims=el.dims, attrs=el.attrs)
    except Exception:
        pass
    return el


# ── check_relationships ───────────────────────────────────────────────────────

def check_relationships(sdata) -> None:
    """Print per-edge statistics for all element_relationships in sdata."""
    rels = sdata.attrs.get("element_relationships", [])
    if not rels:
        print("No element_relationships defined.")
        return

    w = 44
    print(f"  {'Edge':<{w}}  {'Cardinality':<14}  {'Coverage':>9}  Order")
    print("  " + "-" * 85)

    for group in rels:
        if not group:
            continue
        # use full pairwise comparison, anchored to the first element as hub
        hub   = group[0]
        hname = hub["element"]
        htype = _etype(sdata, hname)

        try:
            h_el = sdata[hname]
        except Exception:
            print(f"  {hname}: not found")
            continue

        h_ids = _get_join_ids(h_el, hub["join_strategy"], htype)

        for spoke in group[1:]:
            sname = spoke["element"]
            stype = _etype(sdata, sname)
            label = f"{hname}  →  {sname}"

            try:
                s_el = sdata[sname]
            except Exception:
                print(f"  {label}: not found")
                continue

            s_ids = _get_join_ids(s_el, spoke["join_strategy"], stype)

            if not h_ids or not s_ids:
                print(f"  {label:<{w}}  (IDs not extractable)")
                continue

            shared  = h_ids & s_ids
            n_h, n_s, n_sh = len(h_ids), len(s_ids), len(shared)

            if n_h == n_s == n_sh:
                card = "1:1 ✓"
            elif n_sh == n_h:
                card = f"hub ⊆ spoke"
            elif n_sh == n_s:
                card = f"spoke ⊆ hub"
            else:
                card = f"partial ({n_sh}/{max(n_h, n_s)})"

            coverage = f"{n_sh / max(n_h, n_s) * 100:.1f}%"

            order = "N/A"
            if n_h == n_s == n_sh and hub["join_strategy"] == "index" == spoke["join_strategy"]:
                try:
                    hi = list(h_el.obs.index if htype == "tables" else h_el.index)
                    si = list(s_el.obs.index if stype == "tables" else s_el.index)
                    order = "same ✓" if hi == si else "diff ✗"
                except Exception:
                    pass

            extra = ""
            miss_h = s_ids - h_ids
            miss_s = h_ids - s_ids
            if miss_s:
                extra += f"  [{len(miss_s)} hub-only IDs]"
            if miss_h:
                extra += f"  [{len(miss_h)} spoke-only IDs]"

            print(f"  {label:<{w}}  {card:<14}  {coverage:>9}  {order}{extra}")

    print()


# ── query ─────────────────────────────────────────────────────────────────────

def query(
    sdata,
    element: str,
    depth: Union[int, str] = 1,
    types: Optional[List[str]] = None,
    ids=None,
) -> dict:
    """Retrieve an element and all reachable related elements.

    Parameters
    ----------
    sdata:   SpatialData with element_relationships in attrs.
    element: Root element name.
    depth:   Number of relationship hops, or "all" for full transitive closure.
    types:   Whitelist of element types to include.
             Accepts spatialdata attr names: "shapes", "tables", "labels", "points".
    ids:     Instance IDs to start from.  Each returned element is subset to
             the matching rows, with IDs propagated transitively.

    Returns
    -------
    dict: {element_name: element_data}  — root element always included.
    """
    max_depth = float("inf") if depth == "all" else int(depth)

    type_filter: Optional[set] = None
    if types:
        type_filter = {t.lower() for t in types}

    adj = _rel_graph(sdata)

    # seed
    try:
        root_el = sdata[element]
    except Exception as exc:
        raise KeyError(f"Element '{element}' not found in sdata") from exc

    root_etype  = _etype(sdata, element)
    root_strat  = _strategy_for(sdata, element)
    root_subset = _subset(root_el, root_strat, ids, root_etype) if ids is not None else root_el
    propagate   = ids is not None

    # BFS: (name, subset_element, depth)
    visited: dict[str, object] = {element: root_subset}
    queue: deque = deque([(element, root_subset, 0)])

    while queue:
        curr_name, curr_el, curr_d = queue.popleft()

        if curr_d >= max_depth:
            continue

        curr_etype = _etype(sdata, curr_name)

        for nbr_name, my_strat, nbr_strat in adj.get(curr_name, []):
            if nbr_name in visited:
                continue

            try:
                nbr_raw = sdata[nbr_name]
            except Exception:
                continue

            nbr_etype = _etype(sdata, nbr_name)

            if propagate:
                curr_ids = _get_join_ids(curr_el, my_strat, curr_etype)
                nbr_el   = _subset(nbr_raw, nbr_strat, curr_ids, nbr_etype)
            else:
                nbr_el   = nbr_raw

            visited[nbr_name] = nbr_el
            queue.append((nbr_name, nbr_el, curr_d + 1))

    if type_filter:
        visited = {k: v for k, v in visited.items()
                   if _etype(sdata, k) in type_filter}

    return visited


# ── query_cross ───────────────────────────────────────────────────────────────

def query_cross(
    sdatas,
    root: tuple,
    depth: Union[int, str] = 1,
    types: Optional[List[str]] = None,
    ids=None,
) -> dict:
    """Cross-sdata query following promoted (real) element_relationships edges.

    Parameters
    ----------
    sdatas: list of SpatialData, or dict {name: SpatialData}.
    root:   (sdata_name, element_name) — starting point.
    depth:  Hops within each sdata's relationship graph.
    types:  Optional type whitelist (same as query()).
    ids:    Optional starting IDs.

    Returns
    -------
    dict: {(sdata_name, element_name): element_data}

    Notes
    -----
    Cross-sdata edges (from sjoin_suggestions) are dashed/potential only until
    a spatial join has been computed and its result encoded in element_relationships.
    This function traverses within-sdata edges from the root sdata.
    Extend to multi-sdata once sjoin promotion is available.
    """
    if isinstance(sdatas, list):
        sdatas_dict: dict = {}
        for i, sd in enumerate(sdatas):
            name = getattr(sd, "_name", None) or f"sdata_{i}"
            sdatas_dict[name] = sd
    else:
        sdatas_dict = dict(sdatas)

    root_sdata_name, root_element = root
    if root_sdata_name not in sdatas_dict:
        raise KeyError(
            f"sdata '{root_sdata_name}' not found. "
            f"Available: {list(sdatas_dict)}"
        )

    within = query(
        sdatas_dict[root_sdata_name], root_element,
        depth=depth, types=types, ids=ids,
    )
    return {(root_sdata_name, k): v for k, v in within.items()}
