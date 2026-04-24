"""
Element relationship graph visualizer for SpatialData objects.

Public API
----------
show_graph(sdata, title=None)
    Matplotlib figure: undirected graph, edge labels show join strategy + coverage.

export_html(sdata, output_path, title=None)
    Self-contained interactive HTML using Cytoscape.js.
    Orthogonal taxi edges, dashed sjoin suggestions, click details panel,
    edge-label toggle (metadata / statistics / both / none).

"""

import math
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
from matplotlib.patches import FancyBboxPatch
import numpy as np

# ── colour palette by element type ────────────────────────────────────────────
_C = {
    "Points":  dict(bg="#F0EBF8", hdr="#5C2D91", fg="white"),
    "Shapes":  dict(bg="#EAF3FB", hdr="#174F8C", fg="white"),
    "Labels":  dict(bg="#FEF3E2", hdr="#A84300", fg="white"),
    "Tables":  dict(bg="#E8F6EE", hdr="#1A5C35", fg="white"),
    "Images":  dict(bg="#ECEEF0", hdr="#2F3E4E", fg="white"),
    "Unknown": dict(bg="#F5F5F5", hdr="#555555", fg="white"),
}

_NODE_W = 2.9    # box width  (data units)
_NODE_H = 0.82   # box height
_HDR_H  = 0.27   # header stripe height
_COL_REL   = "#1C2B3A"   # solid relationship edges
_COL_SJOIN = "#B05A00"   # dashed sjoin-suggestion edges
_SPACING_Y = 1.55        # vertical gap between nodes in the same column


# ── element-type detection ────────────────────────────────────────────────────

def _etype(sdata, name):
    for attr, label in [
        ("images", "Images"), ("labels", "Labels"),
        ("points", "Points"), ("shapes", "Shapes"), ("tables", "Tables"),
    ]:
        if name in getattr(sdata, attr, {}):
            return label
    return "Unknown"


def _size_str(sdata, name):
    try:
        if name in sdata.shapes: return f"{len(sdata[name]):,}"
        if name in sdata.tables: return f"{sdata[name].n_obs:,} obs"
        if name in sdata.labels: return "label img"
        if name in sdata.points: return "lazy pts"
        if name in sdata.images: return "image"
    except Exception:
        pass
    return ""


# ── graph construction from attrs ─────────────────────────────────────────────

def _build_graph(sdata):
    """Return (nodes, rel_edges, sjoin_edges, hub, trans_edges) purely from sdata.attrs."""
    nodes = {}   # name → {etype, size}
    rel_edges   = []  # (a, b, label_str) — hub-and-spoke
    trans_edges = []  # (a, b, label_str) — all non-hub pairwise within each group
    sjoin_edges = []  # (a, b)
    hub_elem    = None

    rels   = sdata.attrs.get("element_relationships", [])
    sjoins = sdata.attrs.get("sjoin_suggestions", [])

    # relationship edges: star from first element in each group (hub-and-spoke)
    for group in rels:
        if not group:
            continue
        hub   = group[0]["element"]
        if hub_elem is None:
            hub_elem = hub
        strat   = {d["element"]: d["join_strategy"] for d in group}
        members = [d["element"] for d in group]
        for d in group:
            n = d["element"]
            nodes[n] = None  # filled below
            if n != hub:
                label = f"{strat[hub]} / {strat[n]}"
                rel_edges.append((hub, n, label))
        # transitive: all pairs among non-hub members
        spokes = members[1:]
        for i in range(len(spokes)):
            for j in range(i + 1, len(spokes)):
                a, b = spokes[i], spokes[j]
                trans_edges.append((a, b, f"{strat[a]} / {strat[b]}"))

    # sjoin suggestion edges (within-sdata only; cross-sdata handled by sdata_ann)
    for pair in sjoins:
        if (
            isinstance(pair, (list, tuple))
            and len(pair) == 2
            and isinstance(pair[0], str)
            and isinstance(pair[1], str)
        ):
            a, b = pair[0], pair[1]
            nodes[a] = None
            nodes[b] = None
            sjoin_edges.append((a, b))

    # populate node metadata
    for n in list(nodes):
        nodes[n] = dict(etype=_etype(sdata, n), size=_size_str(sdata, n))

    return nodes, rel_edges, sjoin_edges, hub_elem, trans_edges


# ── layout: 3-column (Points | Shapes+Labels | Tables) ───────────────────────

def _layout(nodes):
    col_assign = {
        "Points": 0, "Shapes": 1, "Labels": 1,
        "Tables": 2, "Images": 1, "Unknown": 1,
    }
    x_coords = {0: 0.0, 1: 4.6, 2: 9.2}
    buckets   = {0: [], 1: [], 2: []}

    for n, info in nodes.items():
        c = col_assign.get(info["etype"], 1)
        buckets[c].append(n)

    pos = {}
    for col, col_nodes in buckets.items():
        x  = x_coords[col]
        n  = len(col_nodes)
        ys = (
            np.linspace((n - 1) * _SPACING_Y / 2, -(n - 1) * _SPACING_Y / 2, n)
            if n > 1 else [0.0]
        )
        for node, y in zip(col_nodes, ys):
            pos[node] = (x, float(y))
    return pos


# ── radial layout (pixel positions) ──────────────────────────────────────────

def _radial_layout_px(nodes, hub=None):
    """Hub at (0,0), spokes equidistant on a circle. Returns {name: (px, py)}."""
    if not nodes:
        return {}
    names = list(nodes)
    if hub is None or hub not in nodes:
        hub = names[0]
    spokes = [n for n in names if n != hub]
    n = len(spokes)
    pos = {hub: (0, 0)}
    if n == 0:
        return pos
    R = max(260, n * 88)                  # pixel radius, scales with spoke count
    for i, name in enumerate(spokes):
        θ = -math.pi / 2 + 2 * math.pi * i / n  # start from top, go clockwise
        pos[name] = (round(R * math.cos(θ)), round(R * math.sin(θ)))
    return pos


# ── edge-endpoint spread + per-edge taxi direction ────────────────────────────

def _spread_endpoints(all_pos, edges):
    """Assign per-edge source/target endpoints and taxi-direction.

    Edges sharing the same side of a node get equispaced connection points
    so they never stack on top of each other.  taxi-direction is set to
    'horizontal' for left/right exits and 'vertical' for top/bottom exits,
    so the first routing segment always leaves perpendicular to the side —
    the combination that Cytoscape renders as clean 90-degree bends.

    all_pos:  {node_id: (px, py)}
    edges:    list of Cytoscape element dicts (with 'data.id/source/target')
    Returns:  {edge_id: {'src': str, 'tgt': str, 'taxiDir': str}}
    """
    from collections import defaultdict
    src_g   = defaultdict(list)   # (node_id, side) → [(edge_id, perp_coord)]
    tgt_g   = defaultdict(list)
    dir_map = {}                  # edge_id → src_side

    for e in edges:
        d = e["data"]
        sid, tid, eid = d["source"], d["target"], d["id"]
        if sid not in all_pos or tid not in all_pos:
            continue
        sx, sy = all_pos[sid]
        tx, ty = all_pos[tid]
        dx, dy = tx - sx, ty - sy
        if abs(dx) >= abs(dy):
            ss, ts = ('right', 'left') if dx >= 0 else ('left', 'right')
            # sort by perpendicular axis (y) so routes don't cross
            src_g[(sid, ss)].append((eid, ty))
            tgt_g[(tid, ts)].append((eid, sy))
        else:
            ss, ts = ('bottom', 'top') if dy >= 0 else ('top', 'bottom')
            src_g[(sid, ss)].append((eid, tx))
            tgt_g[(tid, ts)].append((eid, sx))
        dir_map[eid] = ss

    def ep(side, frac):
        p = f"{frac * 100:.1f}%"
        if side == 'right':  return f"100% {p}"
        if side == 'left':   return f"0% {p}"
        if side == 'bottom': return f"{p} 100%"
        return                      f"{p} 0%"        # top

    src_ep = {}
    for (nid, side), items in src_g.items():
        items.sort(key=lambda x: x[1])
        n = len(items)
        for i, (eid, _) in enumerate(items):
            src_ep[eid] = ep(side, (i + 1) / (n + 1) if n > 1 else 0.5)

    tgt_ep = {}
    for (nid, side), items in tgt_g.items():
        items.sort(key=lambda x: x[1])
        n = len(items)
        for i, (eid, _) in enumerate(items):
            tgt_ep[eid] = ep(side, (i + 1) / (n + 1) if n > 1 else 0.5)

    result = {}
    for eid in dir_map:
        result[eid] = {
            'src': src_ep.get(eid, 'outside-to-node'),
            'tgt': tgt_ep.get(eid, 'outside-to-node'),
        }
    return result


# ── computing data weights ────────────────────────────────────────────────────

def _get_ids(sdata, name, strat_map):
    t, s = strat_map.get(name, (None, "index"))
    try:
        el = sdata[name]
        if t == "dataframe":
            if s == "index":
                return set(el.index.tolist())
            if hasattr(el, "obs") and s in el.obs.columns:
                return set(el.obs[s].tolist())
            if hasattr(el, "columns") and s in el.columns:
                return set(el[s].tolist())
    except Exception:
        pass
    return None


def _compute_weights(sdata, rel_edges, trans_edges=None):
    """Return dict (a, b) → {"statsLabel": str, "orderLabel": str}."""
    rels = sdata.attrs.get("element_relationships", [])
    strat_map = {
        d["element"]: (d["type"], d["join_strategy"])
        for group in rels for d in group
    }
    weights = {}
    for a, b, _ in list(rel_edges) + list(trans_edges or []):
        if (a, b) in weights:
            continue
        ids_a = _get_ids(sdata, a, strat_map)
        ids_b = _get_ids(sdata, b, strat_map)
        if ids_a is None or ids_b is None:
            continue
        shared  = len(ids_a & ids_b)
        n_max   = max(len(ids_a), len(ids_b))
        perfect = len(ids_a) == len(ids_b) == shared
        stats   = ("✓ " if perfect else "") + f"{shared:,}/{n_max:,}"

        order_label = ""
        if perfect:
            _ta, sa = strat_map.get(a, (None, "index"))
            _tb, sb = strat_map.get(b, (None, "index"))
            if sa == "index" and sb == "index":
                try:
                    el_a = sdata[a]; el_b = sdata[b]
                    idx_a = list(el_a.obs.index if hasattr(el_a, "obs") else el_a.index)
                    idx_b = list(el_b.obs.index if hasattr(el_b, "obs") else el_b.index)
                    order_label = "⟷ same order" if idx_a == idx_b else "≋ diff order"
                except Exception:
                    pass

        weights[(a, b)] = {"statsLabel": stats, "orderLabel": order_label}
    return weights


# ── drawing primitives ────────────────────────────────────────────────────────

def _draw_node(ax, name, x, y, etype, size_str):
    c  = _C.get(etype, _C["Unknown"])
    hw = _NODE_W / 2
    hh = _NODE_H / 2

    # body (drawn first so header clips on top)
    ax.add_patch(FancyBboxPatch(
        (x - hw, y - hh), _NODE_W, _NODE_H,
        boxstyle="round,pad=0.06",
        facecolor=c["bg"], edgecolor="#888", linewidth=1.1, zorder=3,
    ))
    # header stripe — clipped to top of box
    ax.add_patch(FancyBboxPatch(
        (x - hw, y + hh - _HDR_H), _NODE_W, _HDR_H,
        boxstyle="round,pad=0.06",
        facecolor=c["hdr"], edgecolor="none", zorder=4,
    ))
    # name text in header
    ax.text(x, y + hh - _HDR_H / 2, name,
            ha="center", va="center", fontsize=8.5,
            fontweight="bold", color=c["fg"], zorder=5)
    # type + size in body
    body_cy = y + hh - _HDR_H - (_NODE_H - _HDR_H) / 2
    label   = f"{etype}"
    if size_str:
        label += f"  ·  {size_str}"
    ax.text(x, body_cy, label,
            ha="center", va="center", fontsize=7.0,
            color="#4a4a4a", zorder=5)


def _draw_edge(ax, x1, y1, x2, y2, color, lw, ls, label=""):
    """Draw an undirected orthogonal L-shaped edge; straight if same column."""
    same_col = abs(x1 - x2) < 0.1

    if same_col:
        ax.plot([x1, x2], [y1, y2], color=color, lw=lw, ls=ls, zorder=2,
                solid_capstyle="round")
        if label:
            ax.text(x1 + 0.15, (y1 + y2) / 2, label,
                    fontsize=6.0, color=color, ha="left", va="center",
                    bbox=dict(fc="white", ec="none", pad=0.8), zorder=4)
    else:
        mid_x = (x1 + x2) / 2
        ax.plot([x1, mid_x, mid_x, x2], [y1, y1, y2, y2],
                color=color, lw=lw, ls=ls, zorder=2,
                solid_capstyle="round", solid_joinstyle="round")
        if label:
            ax.text(mid_x + 0.12, (y1 + y2) / 2, label,
                    fontsize=6.0, color=color, ha="left", va="center",
                    bbox=dict(fc="white", ec="none", pad=0.8), zorder=4)


# ── main axis renderer ────────────────────────────────────────────────────────

def _draw_on_ax(ax, sdata, title=""):
    nodes, rel_edges, sjoin_edges, _hub, _trans = _build_graph(sdata)

    if not nodes:
        ax.text(0.5, 0.5, "No relationships defined",
                ha="center", va="center", transform=ax.transAxes,
                fontsize=10, color="#888")
        ax.axis("off")
        return

    pos     = _layout(nodes)
    weights = _compute_weights(sdata, rel_edges)

    ax.set_facecolor("#F7F8FA")
    ax.axis("off")

    # ── edges (drawn before nodes so boxes sit on top) ────────────────────────
    for a, b, meta_label in rel_edges:
        if a not in pos or b not in pos:
            continue
        x1, y1 = pos[a]
        x2, y2 = pos[b]
        # combine join-strategy description with coverage statistics
        lines = [meta_label]
        if (a, b) in weights:
            w = weights[(a, b)]
            lines.append(w["statsLabel"])
            if w.get("orderLabel"):
                lines.append(w["orderLabel"])
        label = "\n".join(lines)
        _draw_edge(ax, x1, y1, x2, y2,
                   color=_COL_REL, lw=1.5, ls="solid", label=label)

    for a, b in sjoin_edges:
        if a not in pos or b not in pos:
            continue
        x1, y1 = pos[a]
        x2, y2 = pos[b]
        _draw_edge(ax, x1, y1, x2, y2,
                   color=_COL_SJOIN, lw=1.1, ls=(0, (5, 3)), label="sjoin")

    # ── nodes ─────────────────────────────────────────────────────────────────
    for n, (x, y) in pos.items():
        _draw_node(ax, n, x, y, nodes[n]["etype"], nodes[n]["size"])

    # ── title ─────────────────────────────────────────────────────────────────
    if title:
        ax.set_title(title, fontsize=10, fontweight="bold",
                     loc="left", pad=8, color="#1C2B3A")

    # ── legend ────────────────────────────────────────────────────────────────
    legend_handles = [
        mpatches.Patch(fc=v["bg"], ec=v["hdr"], lw=1.1, label=k)
        for k, v in _C.items() if k != "Unknown"
    ]
    legend_handles += [
        mpatches.Patch(fc="none", ec=_COL_REL,  lw=1.5,
                       linestyle="solid",         label="relationship"),
        mpatches.Patch(fc="none", ec=_COL_SJOIN, lw=1.1,
                       linestyle=(0, (5, 3)),      label="sjoin suggestion"),
    ]
    ax.legend(handles=legend_handles, loc="lower right",
              fontsize=7, framealpha=0.95, edgecolor="#ccc", handlelength=2.0)

    # ── axis limits ───────────────────────────────────────────────────────────
    xs = [p[0] for p in pos.values()]
    ys = [p[1] for p in pos.values()]
    ax.set_xlim(min(xs) - 2.2, max(xs) + 2.2)
    ax.set_ylim(min(ys) - 1.3, max(ys) + 1.3)


# ── public API ────────────────────────────────────────────────────────────────

def show_graph(sdata, title=None):
    """Display the element relationship graph for a SpatialData object.

    Each edge shows both the join strategy (from metadata) and the
    data-coverage statistics (computed on the fly from element indices).
    Sjoin-suggestion edges are shown dashed; no arrowheads (undirected).
    """
    fig, ax = plt.subplots(figsize=(13, 7))
    fig.patch.set_facecolor("#F7F8FA")
    _draw_on_ax(ax, sdata, title=title or "")
    plt.tight_layout()
    plt.show()


# ── interactive HTML export ───────────────────────────────────────────────────

def _svg_uri(name, etype, size, hdr_color, bg_color):
    """Pre-compute a data URI for a node's SVG background (called in Python, not JS)."""
    import html as _html
    import urllib.parse
    W   = max(200, min(290, len(name) * 9 + 32))
    H, HDR, R = 64, 26, 7
    ne  = _html.escape(name)
    mid = HDR + (H - HDR) / 2
    compress = f' textLength="{W-22}" lengthAdjust="spacingAndGlyphs"' if len(name) * 9 > W - 32 else ""
    body_text = f"{_html.escape(etype)}{' · ' + _html.escape(size) if size else ''}"
    svg = (
        f'<svg xmlns="http://www.w3.org/2000/svg" width="{W}" height="{H}">'
        f'<rect width="{W}" height="{H}" rx="{R}" fill="{bg_color}" stroke="#9aacbc" stroke-width="1.3"/>'
        f'<rect width="{W}" height="{HDR}" rx="{R}" fill="{hdr_color}"/>'
        f'<rect y="{R}" width="{W}" height="{HDR-R}" fill="{hdr_color}"/>'
        f'<text x="{W/2:.1f}" y="{HDR/2+5:.1f}" text-anchor="middle" fill="white"'
        f' font-family="ui-sans-serif,system-ui,sans-serif" font-size="11.5" font-weight="700"{compress}>{ne}</text>'
        f'<text x="{W/2:.1f}" y="{mid+4:.1f}" text-anchor="middle" fill="#4a5a6a"'
        f' font-family="ui-sans-serif,system-ui,sans-serif" font-size="10">{body_text}</text>'
        f'</svg>'
    )
    return "data:image/svg+xml;charset=utf-8," + urllib.parse.quote(svg)


def export_html(sdata_or_sdatas, output_path, title=None, cross_sdata_file=None):
    """Export a self-contained interactive HTML graph using Cytoscape.js.

    Single sdata:
        export_html(sdata, "out.html", title="my sdata")

    Multiple sdatas (compound-node view, sdatas laid out left-to-right):
        export_html({"sdata_qc": sdata_qc, ...}, "out.html",
                    cross_sdata_file="cross_sdata.json", title="All")
        cross_sdata_file: JSON {"sjoin_suggestions": [[{sdata,element},{sdata,element}], ...]}
    """
    import json

    X_SCALE, Y_SCALE = 200, 85

    def _leaf_node(nid, name, info, px, py):
        c = _C.get(info["etype"], _C["Unknown"])
        width = max(200, min(290, len(name) * 9 + 32))
        return {
            "data": {
                "id":        nid,
                "label":     name,
                "etype":     info["etype"],
                "size":      info["size"],
                "hdrColor":  c["hdr"],
                "bgColor":   c["bg"],
                "bgImage":   _svg_uri(name, info["etype"], info["size"], c["hdr"], c["bg"]),
                "nodeWidth": width,
                "nodeHeight": 64,
            },
            "position": {"x": px, "y": py},
        }

    if isinstance(sdata_or_sdatas, dict):
        # ── multi-sdata path ──────────────────────────────────────────────────
        title = title or "Multi-SpatialData Relationships"
        GROUP_GAP   = 320
        sdata_names = list(sdata_or_sdatas.keys())

        cross_sjoins   = []
        cross_mat_rels = []
        if cross_sdata_file:
            with open(cross_sdata_file) as f:
                _cj = json.load(f)
                cross_sjoins   = _cj.get("sjoin_suggestions", [])
                cross_mat_rels = _cj.get("element_relationships", [])

        # pre-scan so referenced standalone elements (e.g. rois) get nodes
        cross_refs: dict = {}
        for pair in cross_sjoins:
            if len(pair) == 2 and all(isinstance(p, dict) for p in pair):
                for ep in pair:
                    cross_refs.setdefault(ep["sdata"], set()).add(ep["element"])

        compound_nodes = []   # rendered below children by Cytoscape
        leaf_nodes     = []
        within_edges   = []
        portal_nodes   = []
        portal_edges   = []

        node_positions: dict = {}  # {sdata_name: {elem_name: (px, py)}}
        sdata_max_x:   dict = {}   # rightmost leaf-node px per sdata
        x_cursor = 0

        for sdata_name, sdata in sdata_or_sdatas.items():
            nodes_data, rel_edges, sjoin_edges, hub, trans_edges = _build_graph(sdata)

            for extra in cross_refs.get(sdata_name, set()):
                if extra not in nodes_data:
                    nodes_data[extra] = dict(
                        etype=_etype(sdata, extra),
                        size=_size_str(sdata, extra),
                    )

            weights  = _compute_weights(sdata, rel_edges, trans_edges)
            raw_pos  = _radial_layout_px(nodes_data, hub)   # already in pixels

            min_x_raw = min((p[0] for p in raw_pos.values()), default=0)
            max_x_raw = max((p[0] for p in raw_pos.values()), default=0)
            x_span_px = max_x_raw - min_x_raw + 300   # +300 for node widths

            compound_nodes.append({"data": {
                "id":        f"g__{sdata_name}",
                "label":     sdata_name,
                "sdataName": sdata_name,
                "kind":      "group",
                "bgImage":   "",
                "nodeWidth": 150,
                "nodeHeight": 64,
            }})

            node_positions[sdata_name] = {}
            for name, info in nodes_data.items():
                rx, ry = raw_pos.get(name, (0, 0))
                px = rx - min_x_raw + x_cursor + 200
                py = ry
                node_positions[sdata_name][name] = (px, py)
                nd = _leaf_node(f"{sdata_name}__{name}", name, info, px, py)
                nd["data"]["parent"] = f"g__{sdata_name}"
                nd["data"]["sdata"]  = sdata_name
                leaf_nodes.append(nd)

            sdata_max_x[sdata_name] = max(
                (px for px, _ in node_positions[sdata_name].values()), default=x_cursor + 200
            )

            for a, b, meta_label in rel_edges:
                w = weights.get((a, b), {})
                within_edges.append({"data": {
                    "id":         f"r__{sdata_name}__{a}__{b}",
                    "source":     f"{sdata_name}__{a}",
                    "target":     f"{sdata_name}__{b}",
                    "kind":       "rel",
                    "sdata":      sdata_name,
                    "sourceSdata": sdata_name, "targetSdata": sdata_name,
                    "metaLabel":  meta_label,
                    "statsLabel": w.get("statsLabel", ""),
                    "orderLabel": w.get("orderLabel", ""),
                }})
            for a, b, meta_label in trans_edges:
                w = weights.get((a, b), {})
                within_edges.append({"data": {
                    "id":         f"rt__{sdata_name}__{a}__{b}",
                    "source":     f"{sdata_name}__{a}",
                    "target":     f"{sdata_name}__{b}",
                    "kind":       "rel_trans",
                    "sdata":      sdata_name,
                    "sourceSdata": sdata_name, "targetSdata": sdata_name,
                    "metaLabel":  meta_label,
                    "statsLabel": w.get("statsLabel", ""),
                    "orderLabel": w.get("orderLabel", ""),
                }})
            for a, b in sjoin_edges:
                within_edges.append({"data": {
                    "id":         f"s__{sdata_name}__{a}__{b}",
                    "source":     f"{sdata_name}__{a}",
                    "target":     f"{sdata_name}__{b}",
                    "kind":       "sjoin",
                    "sdata":      sdata_name,
                    "sourceSdata": sdata_name, "targetSdata": sdata_name,
                    "metaLabel":  "sjoin", "statsLabel": "", "orderLabel": "",
                }})

            x_cursor += x_span_px + GROUP_GAP

        # ── BFS reachability within a single sdata ───────────────────────────
        def _reachable(sdata_obj, start):
            """All elements reachable from `start` via element_relationships (excl. start)."""
            adj = {}
            for group in sdata_obj.attrs.get("element_relationships", []):
                for d in group:
                    adj.setdefault(d["element"], set())
                for di in group:
                    for dj in group:
                        if di["element"] != dj["element"]:
                            adj[di["element"]].add(dj["element"])
            visited, q = {start}, [start]
            while q:
                for nbr in adj.get(q.pop(0), set()):
                    if nbr not in visited:
                        visited.add(nbr); q.append(nbr)
            visited.discard(start)
            return visited

        # ── cross-sdata portal generation via full BFS ───────────────────────
        # Build undirected adjacency from sjoin suggestions + materialized rels.
        # Each entry: (tgt_sdata, src_elem, tgt_elem, is_materialized, meta_dict)
        cross_adj: dict = {}
        for pair in cross_sjoins:
            if not (len(pair) == 2 and all(isinstance(p, dict) for p in pair)):
                continue
            a, b = pair[0], pair[1]
            cross_adj.setdefault(a["sdata"], []).append(
                (b["sdata"], a["element"], b["element"], False, {}))
            cross_adj.setdefault(b["sdata"], []).append(
                (a["sdata"], b["element"], a["element"], False, {}))
        for rel in cross_mat_rels:
            elems_list = rel.get("elements", [])
            if len(elems_list) < 2:
                continue
            meta = {k: rel.get(k, "") for k in ("metaLabel", "statsLabel", "orderLabel")}
            for i in range(len(elems_list)):
                for j in range(i + 1, len(elems_list)):
                    a, b = elems_list[i], elems_list[j]
                    cross_adj.setdefault(a["sdata"], []).append(
                        (b["sdata"], a["element"], b["element"], True, meta))
                    cross_adj.setdefault(b["sdata"], []).append(
                        (a["sdata"], b["element"], a["element"], True, meta))

        seen_portals      = set()
        seen_portal_edges = set()
        PORTAL_X_GAP   = 270
        PORTAL_Y_STEP  = 52
        portal_y_count: dict = {}

        from collections import deque as _deq
        for start_sdata in list(sdata_or_sdatas.keys()):
            if start_sdata not in node_positions:
                continue
            # BFS queue items: (curr_sdata, src_elem_in_start, tgt_elem, hops, is_mat, meta)
            queue: _deq = _deq()
            for (nbr_sdata, src_elem, nbr_elem, is_mat, meta) in cross_adj.get(start_sdata, []):
                queue.append((nbr_sdata, src_elem, nbr_elem, 1, is_mat, meta))
            visited = {start_sdata}

            while queue:
                curr_sdata, src_elem, tgt_elem, hops, is_mat, meta = queue.popleft()
                if curr_sdata in visited:
                    continue
                visited.add(curr_sdata)
                if curr_sdata not in sdata_or_sdatas:
                    continue

                s_pos = node_positions.get(start_sdata, {}).get(src_elem)
                if s_pos is None:
                    continue
                s_px, s_py = s_pos
                portal_x = sdata_max_x.get(start_sdata, 0) + PORTAL_X_GAP

                is_trans_cross = hops > 1
                # Materialized portals only on direct (1-hop) materialized edges
                is_portal_mat = is_mat and not is_trans_cross

                # within-sdata transitive targets at curr_sdata
                trans_elems = sorted(_reachable(sdata_or_sdatas[curr_sdata], tgt_elem))
                all_targets = [(tgt_elem, False)] + [(e, True) for e in trans_elems]

                for (t_elem_i, is_within_trans) in all_targets:
                    is_any_trans = is_trans_cross or is_within_trans
                    pid  = f"portal__{start_sdata}__{src_elem}__{curr_sdata}__{t_elem_i}"
                    pe_id = f"pe__{start_sdata}__{src_elem}__{curr_sdata}__{t_elem_i}"

                    if pid not in seen_portals:
                        seen_portals.add(pid)
                        key = (start_sdata, src_elem)
                        idx = portal_y_count.get(key, 0)
                        portal_y_count[key] = idx + 1
                        w = max(150, (len(curr_sdata) + len(t_elem_i) + 3) * 7 + 16)
                        portal_nodes.append({"data": {
                            "id":           pid,
                            "label":        f"{curr_sdata}\n{t_elem_i}",
                            "kind":         "portal",
                            "materialized": is_portal_mat and not is_any_trans,
                            "transitive":   is_any_trans,
                            "sdata":        start_sdata,
                            "srcElem":      src_elem,
                            "targetSdata":  curr_sdata,
                            "targetElem":   t_elem_i,
                            "bgImage":      "",
                            "nodeWidth":    w,
                            "nodeHeight":   44,
                        }, "position": {
                            "x": portal_x,
                            "y": s_py + idx * PORTAL_Y_STEP,
                        }})

                    if pe_id not in seen_portal_edges:
                        seen_portal_edges.add(pe_id)
                        portal_edges.append({"data": {
                            "id":           pe_id,
                            "source":       f"{start_sdata}__{src_elem}",
                            "target":       pid,
                            "kind":         "portal_edge",
                            "materialized": is_portal_mat and not is_any_trans,
                            "transitive":   is_any_trans,
                            "sourceSdata":  start_sdata,
                            "targetSdata":  curr_sdata,
                            "metaLabel":    meta.get("metaLabel", "") if not is_any_trans else "",
                            "statsLabel":   meta.get("statsLabel", "") if not is_any_trans else "",
                            "orderLabel":   meta.get("orderLabel", "") if not is_any_trans else "",
                        }})

                # Continue BFS to find further hops
                for (nbr_sdata, _, nbr_elem, nbr_mat, nbr_meta) in cross_adj.get(curr_sdata, []):
                    if nbr_sdata not in visited:
                        queue.append((nbr_sdata, src_elem, nbr_elem, hops + 1,
                                      is_mat and nbr_mat, {}))

        # endpoints are computed dynamically in JS from node positions

        # compound nodes first — Cytoscape renders them behind their children
        elements = compound_nodes + leaf_nodes + within_edges + portal_nodes + portal_edges

    else:
        # ── single-sdata path ─────────────────────────────────────────────────
        title = title or "Element Relationships"
        sdata = sdata_or_sdatas
        nodes_data, rel_edges, sjoin_edges, hub, trans_edges = _build_graph(sdata)
        weights  = _compute_weights(sdata, rel_edges, trans_edges)
        raw_pos  = _radial_layout_px(nodes_data, hub)
        min_x_raw = min((p[0] for p in raw_pos.values()), default=0)

        node_elems = []
        edge_elems = []
        for name, info in nodes_data.items():
            rx, ry = raw_pos.get(name, (0, 0))
            px = rx - min_x_raw + 200
            node_elems.append(_leaf_node(name, name, info, px, ry))

        for a, b, meta_label in rel_edges:
            w = weights.get((a, b), {})
            edge_elems.append({"data": {
                "id": f"r__{a}__{b}", "source": a, "target": b,
                "kind": "rel", "metaLabel": meta_label,
                "statsLabel": w.get("statsLabel", ""), "orderLabel": w.get("orderLabel", ""),
            }})
        for a, b, meta_label in trans_edges:
            w = weights.get((a, b), {})
            edge_elems.append({"data": {
                "id": f"rt__{a}__{b}", "source": a, "target": b,
                "kind": "rel_trans", "sdata": "single", "metaLabel": meta_label,
                "statsLabel": w.get("statsLabel", ""), "orderLabel": w.get("orderLabel", ""),
            }})
        for a, b in sjoin_edges:
            edge_elems.append({"data": {
                "id": f"s__{a}__{b}", "source": a, "target": b,
                "kind": "sjoin", "metaLabel": "sjoin", "statsLabel": "", "orderLabel": "",
            }})

        # endpoints are computed dynamically in JS from node positions

        elements = node_elems + edge_elems

    elements_json   = json.dumps(elements, indent=2)
    sdata_names_var = json.dumps(sdata_names if isinstance(sdata_or_sdatas, dict) else [])
    html = (
        _HTML_TEMPLATE
        .replace("__ELEMENTS__",   elements_json)
        .replace("__SDATA_NAMES__", sdata_names_var)
        .replace("__TITLE__",      title)
    )
    with open(output_path, "w") as fh:
        fh.write(html)
    print(f"Saved: {output_path}")


_HTML_TEMPLATE = r"""<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>__TITLE__</title>
<style>
*{box-sizing:border-box;margin:0;padding:0}
body{display:flex;height:100vh;font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif;background:#0f1923;overflow:hidden}

/* ── sidebar ── */
#sidebar{width:268px;min-width:268px;background:#16202e;color:#c8daea;display:flex;flex-direction:column;border-right:1px solid #243040;overflow:hidden}
.s-header{padding:18px 16px 14px;border-bottom:1px solid #243040;flex-shrink:0}
.s-header h1{font-size:14px;font-weight:700;color:#eaf4ff;letter-spacing:.3px;margin-bottom:3px}
.s-header .sub{font-size:10.5px;color:#5a7a9a}
.s-section{padding:12px 16px;border-bottom:1px solid #243040;flex-shrink:0}
.s-label{font-size:9.5px;font-weight:700;text-transform:uppercase;letter-spacing:.9px;color:#4a7090;margin-bottom:8px}
.btn-group{display:flex;gap:3px;margin-bottom:9px}
.btn{flex:1;padding:5px 3px;font-size:10px;font-weight:600;border:1px solid #253545;border-radius:4px;background:#1e2e3e;color:#7a9dbb;cursor:pointer;transition:all .15s}
.btn:hover{background:#283d52;color:#aac8e8}
.btn.on{background:#1a5282;border-color:#2874b8;color:#d8eeff}
.btn-fit{width:100%;padding:6px;font-size:10.5px;font-weight:600;border:1px solid #253545;border-radius:4px;background:#1e2e3e;color:#7a9dbb;cursor:pointer;transition:all .15s;margin-top:3px}
.btn-fit:hover{background:#283d52;color:#aac8e8}
/* datasets radio buttons */
.sd-item{display:flex;align-items:center;gap:7px;padding:4px 0;font-size:11.5px;color:#a0bdd8;cursor:pointer;border-radius:3px;user-select:none}
.sd-item:hover{color:#d0e8ff}
.sd-item input[type=radio]{width:13px;height:13px;cursor:pointer;accent-color:#2874b8;flex-shrink:0}
.sd-eye{margin-left:auto;background:none;border:none;cursor:pointer;color:#3a5a78;font-size:13px;padding:0 2px;line-height:1;transition:color .15s;flex-shrink:0}
.sd-eye:hover{color:#7ab8e8}
.sd-eye.off{color:#243040}
.sd-eye.off svg path[d*="M12"]{display:none}
#info{flex:1;padding:14px 16px;overflow-y:auto;border-bottom:1px solid #243040;min-height:0}
.i-empty{font-size:11px;color:#3d5a72;font-style:italic}
.i-name{font-size:14px;font-weight:700;color:#ddeeff;margin-bottom:2px}
.i-type{font-size:11px;color:#5a8aaa;margin-bottom:10px}
.i-sec{font-size:9.5px;font-weight:700;text-transform:uppercase;letter-spacing:.7px;color:#4a7090;margin:10px 0 5px}
.i-row{display:flex;justify-content:space-between;padding:3px 0;border-bottom:1px solid #1a2a38;font-size:11px}
.i-key{color:#6a9abb}.i-val{color:#b8d8f8;font-weight:600}
.i-conn{padding:4px 0;border-bottom:1px solid #1a2a38;font-size:11px}
.i-cname{color:#c8e4ff;font-weight:600}
.i-cstrat{color:#4a7090;font-size:10px}
#legend{padding:12px 16px;flex-shrink:0}
.leg-item{display:flex;align-items:center;gap:6px;margin-bottom:5px;font-size:10.5px;color:#6a8aaa}
.leg-sw{width:13px;height:13px;border-radius:3px;flex-shrink:0}
.leg-line{width:22px;height:2px;background:#1C2B3A;flex-shrink:0}
.leg-dash{width:22px;height:2px;flex-shrink:0;background:repeating-linear-gradient(90deg,#B05A00 0,#B05A00 5px,transparent 5px,transparent 9px)}

/* ── canvas ── */
#cy{flex:1;height:100vh;background:#f3f6fa;background-image:radial-gradient(circle,#c8d4e0 1px,transparent 1px);background-size:28px 28px}
</style>
</head>
<body>

<div id="sidebar">
  <div class="s-header">
    <h1>__TITLE__</h1>
    <div class="sub">Element relationship graph</div>
  </div>

  <div class="s-section" id="datasets-section" style="display:none">
    <div class="s-label">Datasets</div>
    <div id="sd-list"></div>
    <label class="sd-item" id="transitive-row" style="margin-top:8px;display:none">
      <input type="checkbox" id="cb-transitive" onchange="setShowTransitive(this.checked)">
      <span>Show transitive links</span>
    </label>
  </div>

  <div class="s-section">
    <div class="s-label">Edge labels</div>
    <div class="btn-group">
      <button class="btn"    id="bm" onclick="setMode('meta')" >Metadata</button>
      <button class="btn on" id="bb" onclick="setMode('both')" >Both</button>
      <button class="btn"    id="bs" onclick="setMode('stats')">Stats</button>
      <button class="btn"    id="bn" onclick="setMode('none')" >None</button>
    </div>
    <button class="btn-fit" onclick="fitVisible()">↕ Fit graph</button>
    <button class="btn-fit" onclick="resetLayout()" style="margin-top:4px">⟳ Reset layout</button>
  </div>

  <div id="info">
    <div class="s-label">Details</div>
    <div id="info-body" class="i-empty">Hover or click a node or edge</div>
  </div>

  <div id="legend">
    <div class="s-label">Legend</div>
    <div id="leg-items"></div>
  </div>
</div>

<div id="cy"></div>

<script src="https://cdnjs.cloudflare.com/ajax/libs/cytoscape/3.28.1/cytoscape.min.js"></script>
<script>
const ELEMENTS    = __ELEMENTS__;
const SDATA_NAMES = __SDATA_NAMES__;

const TC = {
  Points: {hdr:'#5C2D91',bg:'#F0EBF8'},
  Shapes: {hdr:'#174F8C',bg:'#EAF3FB'},
  Labels: {hdr:'#A84300',bg:'#FEF3E2'},
  Tables: {hdr:'#1A5C35',bg:'#E8F6EE'},
  Images: {hdr:'#2F3E4E',bg:'#ECEEF0'},
  Unknown:{hdr:'#555555',bg:'#F5F5F5'},
};

// ── legend ────────────────────────────────────────────────────────────────────
const legEl = document.getElementById('leg-items');
Object.entries(TC).forEach(([t,c])=>{
  legEl.innerHTML+=`<div class="leg-item"><div class="leg-sw" style="background:${c.hdr}"></div><span>${t}</span></div>`;
});
legEl.innerHTML+=`
  <div class="leg-item" style="margin-top:6px"><div class="leg-line" style="background:#1a7a46"></div><span>Relationship — full match</span></div>
  <div class="leg-item"><div class="leg-line"></div><span>Relationship — partial</span></div>
  <div class="leg-item"><div class="leg-line" style="background:#4a6a88;height:2px;opacity:0.7"></div><span>Transitive link</span></div>
  <div class="leg-item"><div class="leg-dash"></div><span>Sjoin suggestion</span></div>`;

// ── dataset radio buttons ─────────────────────────────────────────────────────
let activeSdatas  = new Set(SDATA_NAMES.length ? [SDATA_NAMES[0]] : []);
const hiddenTargets = new Set();

// Eye SVG icons (open / closed)
const _eyeOpen   = `<svg width="14" height="10" viewBox="0 0 14 10" fill="none" stroke="currentColor" stroke-width="1.4" stroke-linecap="round"><path d="M1 5C2.5 2 4.5 1 7 1s4.5 1 6 4c-1.5 3-3.5 4-6 4S2.5 8 1 5z"/><circle cx="7" cy="5" r="1.8" fill="currentColor" stroke="none"/></svg>`;
const _eyeClosed = `<svg width="14" height="10" viewBox="0 0 14 10" fill="none" stroke="currentColor" stroke-width="1.4" stroke-linecap="round"><path d="M1 5C2.5 2 4.5 1 7 1s4.5 1 6 4c-1.5 3-3.5 4-6 4S2.5 8 1 5z" stroke-opacity="0.3"/><line x1="2" y1="9" x2="12" y2="1"/></svg>`;

function toggleHide(name, ev) {
  ev.stopPropagation(); ev.preventDefault();
  const btn = ev.currentTarget;
  if (hiddenTargets.has(name)) {
    hiddenTargets.delete(name);
    btn.innerHTML = _eyeOpen;
    btn.classList.remove('off');
    btn.title = 'Hide as link target';
  } else {
    hiddenTargets.add(name);
    btn.innerHTML = _eyeClosed;
    btn.classList.add('off');
    btn.title = 'Show as link target';
  }
  updateVisibility();
}

if (SDATA_NAMES.length > 1) {
  document.getElementById('datasets-section').style.display = '';
  document.getElementById('transitive-row').style.display = '';
  const listEl = document.getElementById('sd-list');
  SDATA_NAMES.forEach((name, i) => {
    const checked = i === 0 ? 'checked' : '';
    listEl.innerHTML += `<label class="sd-item">` +
      `<input type="radio" name="sdata_radio" value="${name}" ${checked} onchange="selectSdata('${name}')">` +
      `<span>${name}</span>` +
      `<button class="sd-eye" onclick="toggleHide('${name}', event)" title="Hide as link target">${_eyeOpen}</button>` +
      `</label>`;
  });
}

let showTransitive = false;

function selectSdata(name) {
  activeSdatas = new Set([name]);
  updateVisibility();
}

function activateSdata(name) {
  activeSdatas = new Set([name]);
  const rb = document.querySelector('input[name="sdata_radio"][value="' + name + '"]');
  if (rb) rb.checked = true;
  updateVisibility();
}

function setShowTransitive(on) {
  showTransitive = on;
  updateVisibility();
}

function updateVisibility() {
  if (SDATA_NAMES.length > 0) {
    // compound group nodes — hiding them hides all children automatically
    cy.nodes('[kind="group"]').forEach(n => {
      n.style('display', activeSdatas.has(n.data('sdataName')) ? 'element' : 'none');
    });

    // portal nodes: show when source active, target not active/hidden; respect transitive toggle
    cy.nodes('[kind="portal"]').forEach(n => {
      const src = n.data('sdata'), tgt = n.data('targetSdata');
      const trans = n.data('transitive'), mat = n.data('materialized');
      const show = activeSdatas.has(src)
        && !activeSdatas.has(tgt)
        && !hiddenTargets.has(tgt)
        && (mat || !trans || showTransitive);
      n.style('display', show ? 'element' : 'none');
    });

    // portal edges: same condition
    cy.edges('[kind="portal_edge"]').forEach(e => {
      const src = e.data('sourceSdata'), tgt = e.data('targetSdata');
      const trans = e.data('transitive'), mat = e.data('materialized');
      const show = activeSdatas.has(src)
        && !activeSdatas.has(tgt)
        && !hiddenTargets.has(tgt)
        && (mat || !trans || showTransitive);
      e.style('display', show ? 'element' : 'none');
    });
  }

  // rel_trans edges: visible only when showTransitive (and sdata is active in multi-sdata mode)
  cy.edges('[kind="rel_trans"]').forEach(e => {
    const sdata = e.data('sdata');
    const show = showTransitive && (!SDATA_NAMES.length || activeSdatas.has(sdata));
    e.style('display', show ? 'element' : 'none');
  });

  fitVisible();
  _renderEdges();
}

function fitVisible() {
  // fit to visible leaf nodes (not compound parents which expand to fit children)
  const vis = cy.nodes(':visible').not('[kind="group"]');
  if (vis.length) cy.fit(vis, 60);
}

// ── edge label mode ───────────────────────────────────────────────────────────
let mode = 'both';
function edgeLabelText(e, d) {
  if (d.kind === 'portal_edge' && !d.materialized) return '';
  if (d.kind === 'sjoin') return 'sjoin';

  // For rel/rel_trans: anchor each strategy to its node name so the label is
  // unambiguous regardless of how nodes are positioned.
  let metaStr = d.metaLabel || '';
  if ((d.kind === 'rel' || d.kind === 'rel_trans') && d.metaLabel) {
    const parts = d.metaLabel.split(' / ');
    if (parts.length === 2) {
      const srcLbl = e.source().data('label') || '?';
      const tgtLbl = e.target().data('label') || '?';
      metaStr = `${srcLbl}: ${parts[0]}\n${tgtLbl}: ${parts[1]}`;
    }
  }

  if (mode === 'meta')  return metaStr;
  if (mode === 'stats') return d.statsLabel || '';
  if (mode === 'both') {
    let t = d.statsLabel ? metaStr + '\n' + d.statsLabel : metaStr;
    if (d.orderLabel) t += '\n' + d.orderLabel;
    return t.replace(/^\n/, '');
  }
  return '';
}
function setMode(m) {
  mode = m;
  ['m','b','s','n'].forEach((k,i) => {
    document.getElementById('b'+k).classList.toggle('on', ['meta','both','stats','none'][i] === m);
  });
  _renderEdges();
}

// ── Cytoscape ─────────────────────────────────────────────────────────────────
let cy;
try {
cy = cytoscape({
  container: document.getElementById('cy'),
  elements: ELEMENTS,
  style: [
    // ── leaf nodes ────────────────────────────────────────────────────────────
    {
      selector: 'node',
      style: {
        'width':            'data(nodeWidth)',
        'height':           'data(nodeHeight)',
        'shape':            'roundrectangle',
        'background-color': '#eaf3fb',
        'background-image': 'data(bgImage)',
        'background-fit':   'cover',
        'background-clip':  'node',
        'border-width':     0,
        'label':            '',
      }
    },
    // ── compound group nodes — auto-sized by Cytoscape around children ────────
    {
      selector: 'node[kind="group"]',
      style: {
        'background-color':   'rgba(220,232,248,0.45)',
        'background-image':   'none',
        'border-width':       1.5,
        'border-color':       '#7a9dc0',
        'label':              'data(label)',
        'text-valign':        'top',
        'text-halign':        'center',
        'text-margin-y':      -8,
        'font-size':          13,
        'font-weight':        700,
        'color':              '#1a3050',
        'shape':              'roundrectangle',
        'padding':            '50px',
        'compound-sizing-wrt-labels': 'include',
      }
    },
    // ── portal stub nodes ─────────────────────────────────────────────────────
    {
      selector: 'node[kind="portal"]',
      style: {
        'background-color': 'rgba(176,90,0,0.15)',
        'background-image': 'none',
        'border-width':     1.5,
        'border-color':     '#B05A00',
        'border-style':     'dashed',
        'label':            'data(label)',
        'text-valign':      'center',
        'text-halign':      'center',
        'text-wrap':        'wrap',
        'font-size':        9.5,
        'font-weight':      700,
        'color':            '#3d1a00',
        'shape':            'roundrectangle',
        'width':            'data(nodeWidth)',
        'height':           'data(nodeHeight)',
        'cursor':           'pointer',
      }
    },
    {
      selector: 'node[kind="portal"][?transitive]',
      style: {
        'background-color': 'rgba(176,90,0,0.07)',
        'border-color':     '#c88040',
        'color':            '#7a3c00',
        'font-weight':      400,
      }
    },
    {
      selector: 'node[kind="portal"][?materialized]',
      style: {
        'background-color': 'rgba(42,62,86,0.12)',
        'border-color':     '#2a3e56',
        'border-style':     'solid',
        'border-width':     2,
        'color':            '#1a2a3a',
        'font-weight':      700,
      }
    },
    {selector:'node:selected',       style:{'border-width':2.5,'border-color':'#2288ee'}},
    {selector:'node.dim',            style:{'opacity':0.22}},
    {selector:'node.hi',             style:{'border-width':2,'border-color':'#2288ee'}},
    // ── edges: invisible in Cytoscape — drawn by SVG overlay ─────────────────
    {
      selector: 'edge',
      style: { 'opacity': 0, 'width': 1, 'label': '' }
    },
  ],
  layout: {name:'preset', fit:true, padding:60},
  wheelSensitivity: 0.25,
  minZoom: 0.08,
  maxZoom: 4,
});
} catch(err) {
  document.getElementById('cy').style.cssText='display:flex;align-items:center;justify-content:center;font-family:monospace;font-size:13px;color:red;background:#fff';
  document.getElementById('cy').textContent='Cytoscape init error: ' + err.message;
  throw err;
}

// ── details panel ─────────────────────────────────────────────────────────────
function setInfo(html) { document.getElementById('info-body').innerHTML = html; }

function nodeInfoHtml(n) {
  const d = n.data();
  if (d.kind === 'portal') {
    const kind = d.materialized ? 'Materialized cross-sdata join' :
                 d.transitive   ? 'Transitive cross-sdata link'  : 'Cross-sdata link (sjoin suggestion)';
    const matBadge = d.materialized
      ? `<span style="display:inline-block;margin-top:4px;padding:1px 6px;background:#2a3e56;color:#d0e8ff;border-radius:3px;font-size:9.5px;font-weight:700">✓ materialized</span>` : '';
    return `<div class="i-name">${d.targetElem || d.label}</div>
      <div class="i-type">${kind}<br><span style="color:#4a7090;font-size:10px">${d.targetSdata}</span>${matBadge ? '<br>' + matBadge : ''}</div>
      <div class="i-sec">Source</div>
      <div class="i-conn"><span class="i-cname">${d.srcElem || '?'}</span><br><span class="i-cstrat">${d.sdata}</span></div>
      <div class="i-conn"><span class="i-cstrat">Click to switch to <strong style="color:#d0e8ff">${d.targetSdata}</strong></span></div>`;
  }
  const relE = n.connectedEdges('[kind="rel"]');
  const sjE  = n.connectedEdges('[kind="sjoin"]').not('[kind="portal_edge"]');
  let h = `<div class="i-name">${d.label}</div>`;
  h += `<div class="i-type">${d.etype||''}${d.size ? ' · ' + d.size : ''}${d.sdata ? '<br><span style="color:#4a7090;font-size:10px">'+d.sdata+'</span>' : ''}</div>`;
  if (relE.length) {
    h += `<div class="i-sec">Relationships</div>`;
    relE.forEach(ed => {
      const peer = ed.connectedNodes().not(n), de = ed.data();
      h += `<div class="i-conn"><span class="i-cname">${peer.data('label')}</span><br>
        <span class="i-cstrat">${de.metaLabel}${de.statsLabel ? ' &nbsp;·&nbsp; ' + de.statsLabel : ''}</span></div>`;
    });
  }
  if (sjE.length) {
    h += `<div class="i-sec">Sjoin suggestions</div>`;
    sjE.forEach(ed => {
      const peer = ed.connectedNodes().not(n);
      h += `<div class="i-conn"><span class="i-cname">${peer.data('label')}</span></div>`;
    });
  }
  return h;
}

function edgeInfoHtml(ed) {
  const d = ed.data();
  if (d.kind === 'portal_edge' && !d.materialized) return null;
  const ns = ed.connectedNodes();
  const a = ns[0].data('label'), b = ns[1].data('label');
  let h = `<div class="i-name">${a} — ${b}</div>`;
  const typeLabel = d.materialized ? 'Materialized cross-sdata join'
                  : d.kind === 'rel_trans' ? 'Transitive relationship'
                  : d.kind === 'sjoin'     ? 'Sjoin suggestion'
                  : 'Relationship';
  h += `<div class="i-type">${typeLabel}${d.sdata ? '<br><span style="color:#4a7090;font-size:10px">'+d.sdata+'</span>' : ''}</div>`;
  if (d.kind === 'rel' || d.kind === 'rel_trans' || d.materialized) {
    if (d.metaLabel) {
      h += `<div class="i-sec">Join key</div>`;
      h += `<div class="i-conn"><span class="i-cstrat" style="font-size:12px;color:#8aadcc">${d.metaLabel}</span></div>`;
    }
    if (d.statsLabel) {
      const raw = d.statsLabel.replace('✓ ','');
      const [s,t] = raw.split('/').map(v => parseInt(v.replace(/,/g,'')));
      const pct = t ? (s/t*100).toFixed(1) : '?';
      h += `<div class="i-sec">Coverage</div>`;
      h += `<div class="i-row"><span class="i-key">shared / total</span><span class="i-val">${d.statsLabel}</span></div>`;
      h += `<div class="i-row"><span class="i-key">coverage</span><span class="i-val">${pct}%</span></div>`;
      h += `<div class="i-row"><span class="i-key">1-to-1</span><span class="i-val">${d.statsLabel.startsWith('✓') ? '✓ yes' : '✗ partial'}</span></div>`;
    }
    if (d.orderLabel) {
      h += `<div class="i-row"><span class="i-key">order</span><span class="i-val">${d.orderLabel}</span></div>`;
    }
  }
  return h;
}

let pinned = false;
const EMPTY_INFO = '<div class="i-empty">Hover or click a node or edge</div>';

// ── SVG edge overlay ──────────────────────────────────────────────────────────
// All edge routing is done here in JS — bypasses Cytoscape's broken taxi+endpoint combo.

const _NS = 'http://www.w3.org/2000/svg';
const _cyCont = cy.container();
_cyCont.style.position = 'relative';

const _svg = document.createElementNS(_NS, 'svg');
_svg.style.cssText = 'position:absolute;top:0;left:0;width:100%;height:100%;pointer-events:none;overflow:visible';
_cyCont.appendChild(_svg);

// Arrowhead markers
_svg.insertAdjacentHTML('afterbegin',
  '<defs>' +
  '<marker id="arrS" markerWidth="7" markerHeight="7" refX="6" refY="3.5" orient="auto">' +
  '<path d="M0,0.5 L6,3.5 L0,6.5 Z" fill="#B05A00"/></marker>' +
  '<marker id="arrT" markerWidth="7" markerHeight="7" refX="6" refY="3.5" orient="auto">' +
  '<path d="M0,0.5 L6,3.5 L0,6.5 Z" fill="#c88040"/></marker>' +
  '</defs>');

const _layer = document.createElementNS(_NS, 'g');
_svg.appendChild(_layer);

// Sync SVG viewport transform with Cytoscape pan/zoom (called cheaply on every render)
function _syncVP() {
  const {x, y} = cy.pan(), z = cy.zoom();
  _layer.setAttribute('transform', `translate(${x},${y}) scale(${z})`);
}

// Model-space bounding box of a node (no labels, no overlays)
function _bb(n) { return n.boundingBox({includeLabels: false, includeOverlays: false}); }

// Model-space point on a node's side at fraction frac ∈ (0,1)
function _sidePt(n, side, frac) {
  const b = _bb(n), w = b.x2 - b.x1, h = b.y2 - b.y1;
  if (side === 'L') return [b.x1,           b.y1 + frac * h];
  if (side === 'R') return [b.x2,           b.y1 + frac * h];
  if (side === 'T') return [b.x1 + frac*w,  b.y1            ];
  /* B */           return [b.x1 + frac*w,  b.y2            ];
}

// True if an edge should be rendered: not display:none, and parent group not hidden
function _edgeShown(e) {
  if (e.style('display') === 'none') return false;
  const pg = e.source().data('parent');
  return !pg || cy.getElementById(pg).style('display') !== 'none';
}

// Group all visible edges by (nodeId, side) and assign equispaced fractions.
// Returns {edgeId: {sS, sF, sN, tS, tF, tN}}
function _computeEPs() {
  const groups = {};  // 'nodeId:side' → [{eid, perp, isS, side, node}]

  cy.edges().filter(_edgeShown).forEach(e => {
    const s = e.source(), t = e.target();
    if (s.data('kind') === 'group' || t.data('kind') === 'group') return;
    const sp = s.position(), tp = t.position();
    const dx = tp.x - sp.x, dy = tp.y - sp.y;

    let sS, tS;
    if (Math.abs(dx) >= Math.abs(dy)) {
      sS = dx >= 0 ? 'R' : 'L';  tS = dx >= 0 ? 'L' : 'R';
    } else {
      sS = dy >= 0 ? 'B' : 'T';  tS = dy >= 0 ? 'T' : 'B';
    }

    // perpendicular coord used for sorting (avoids crossing)
    const sp_perp = (sS === 'L' || sS === 'R') ? tp.y : tp.x;
    const tp_perp = (tS === 'L' || tS === 'R') ? sp.y : sp.x;

    const sk = s.id() + ':' + sS, tk = t.id() + ':' + tS;
    (groups[sk] = groups[sk] || []).push({eid: e.id(), perp: sp_perp, isS: true,  side: sS, node: s});
    (groups[tk] = groups[tk] || []).push({eid: e.id(), perp: tp_perp, isS: false, side: tS, node: t});
  });

  const eps = {};
  Object.values(groups).forEach(items => {
    items.sort((a, b) => a.perp - b.perp);
    const n = items.length;
    items.forEach((it, i) => {
      eps[it.eid] = eps[it.eid] || {};
      const f = (i + 1) / (n + 1);  // equispaced: never 0 or 1
      if (it.isS) { eps[it.eid].sS = it.side; eps[it.eid].sF = f; eps[it.eid].sN = it.node; eps[it.eid].sR = i; eps[it.eid].sC = n; }
      else        { eps[it.eid].tS = it.side; eps[it.eid].tF = f; eps[it.eid].tN = it.node; eps[it.eid].tR = i; eps[it.eid].tC = n; }
    });
  });
  return eps;
}

// Build SVG 'd' string for an orthogonal Z-path.
// Exits source in the direction dictated by sS, bends at mid, arrives at target.
function _orthD(sx, sy, sS, tx, ty, tS) {
  if (sS === 'R' || sS === 'L') {
    // Primary axis: horizontal
    let mx = (sx + tx) / 2;
    if (sS === tS) mx = sS === 'R' ? Math.max(sx, tx) + 70 : Math.min(sx, tx) - 70;
    return `M${sx},${sy}L${mx},${sy}L${mx},${ty}L${tx},${ty}`;
  } else {
    // Primary axis: vertical
    let my = (sy + ty) / 2;
    if (sS === tS) my = sS === 'B' ? Math.max(sy, ty) + 50 : Math.min(sy, ty) - 50;
    return `M${sx},${sy}L${sx},${my}L${tx},${my}L${tx},${ty}`;
  }
}

// Per-edge SVG element cache
const _pels = {};  // eid → {hit, line, textEls}

function _edgeSt(d) {
  const p   = d.kind === 'portal_edge', s = d.kind === 'sjoin';
  const tr  = d.transitive, mat = d.materialized, rt = d.kind === 'rel_trans';
  // perfect coverage: rel/rel_trans edges AND materialized portal edges
  const ok  = (mat || (!p && !s)) && (d.statsLabel || '').startsWith('✓');
  // sjoin-suggestion portals keep orange; everything else uses relationship colours
  const sjoiny = p && !mat;
  return {
    color:   sjoiny ? (tr ? '#c88040' : '#B05A00') : s ? '#B05A00'
             : ok ? '#1a7a46' : rt ? '#4a6a88' : '#2a3e56',
    hiColor: ok ? '#0faa5e' : '#1a88ee',
    width:   sjoiny ? (tr ? 1 : 1.5) : s ? 1.4 : ok ? 2.4 : rt ? 1.2 : 1.8,
    dash:    (sjoiny || s) ? '7 4' : null,
    opacity: (sjoiny && tr) ? 0.65 : rt ? 0.7 : 1,
    arrow:   p ? (tr ? 'url(#arrT)' : 'url(#arrS)') : null,
    glow:    ok,
  };
}

function _renderEdges() {
  const eps  = _computeEPs();
  const seen = new Set();

  cy.edges().forEach(e => {
    const eid = e.id(), d = e.data();
    const vis = _edgeShown(e);

    if (!vis) {
      const p = _pels[eid];
      if (p) { p.glow.remove(); p.hit.remove(); p.line.remove(); p.textEls.forEach(el => el.remove()); delete _pels[eid]; }
      return;
    }
    seen.add(eid);

    const ep = eps[eid];
    if (!ep || !ep.sN || !ep.tN) return;

    const [sx, sy] = _sidePt(ep.sN, ep.sS, ep.sF);
    const [tx, ty] = _sidePt(ep.tN, ep.tS, ep.tF);
    const pathD    = _orthD(sx, sy, ep.sS, tx, ty, ep.tS);
    const st       = _edgeSt(d);

    // Create SVG elements on first encounter
    if (!_pels[eid]) {
      const glow = document.createElementNS(_NS, 'path');
      glow.setAttribute('fill', 'none');
      glow.setAttribute('vector-effect', 'non-scaling-stroke');
      glow.style.pointerEvents = 'none';

      const hit = document.createElementNS(_NS, 'path');
      hit.setAttribute('stroke', 'transparent');
      hit.setAttribute('stroke-width', 10);
      hit.setAttribute('fill', 'none');
      hit.setAttribute('vector-effect', 'non-scaling-stroke');
      hit.style.pointerEvents = 'stroke';
      hit.style.cursor = 'pointer';

      const line = document.createElementNS(_NS, 'path');
      line.setAttribute('fill', 'none');
      line.setAttribute('vector-effect', 'non-scaling-stroke');
      line.style.pointerEvents = 'none';

      _layer.appendChild(glow);
      _layer.appendChild(hit);
      _layer.appendChild(line);
      _pels[eid] = {glow, hit, line, textEls: [], hi: false};

      const eRef = e;
      hit.addEventListener('mouseenter', () => {
        _pels[eid].hi = true;
        const s2 = _edgeSt(eRef.data());
        line.setAttribute('stroke', s2.hiColor);
        line.setAttribute('stroke-width', s2.width + 0.8);
        if (!pinned) { const h = edgeInfoHtml(eRef); if (h) setInfo(h); }
      });
      hit.addEventListener('mouseleave', () => {
        _pels[eid].hi = false;
        const s2 = _edgeSt(eRef.data());
        line.setAttribute('stroke', s2.color);
        line.setAttribute('stroke-width', s2.width);
        if (!pinned) setInfo(EMPTY_INFO);
      });
      hit.addEventListener('click', ev => {
        ev.stopPropagation();
        const h = edgeInfoHtml(eRef);
        if (h) { pinned = true; setInfo(h); }
      });
    }

    const {glow, hit, line} = _pels[eid];
    hit.setAttribute('d', pathD);
    line.setAttribute('d', pathD);
    line.setAttribute('stroke', _pels[eid].hi ? st.hiColor : st.color);
    line.setAttribute('stroke-width', st.width);
    line.setAttribute('opacity', st.opacity);
    if (st.dash)  line.setAttribute('stroke-dasharray', st.dash);
    else          line.removeAttribute('stroke-dasharray');
    if (st.arrow) line.setAttribute('marker-end', st.arrow);
    else          line.removeAttribute('marker-end');

    // Glow halo: wider semi-transparent path drawn behind the line
    if (st.glow) {
      glow.setAttribute('d', pathD);
      glow.setAttribute('stroke', st.color);
      glow.setAttribute('stroke-width', st.width + 5);
      glow.setAttribute('opacity', 0.18);
    } else {
      glow.removeAttribute('d');
    }

    // Labels — recompute on each call (mode may have changed)
    _pels[eid].textEls.forEach(el => el.remove());
    _pels[eid].textEls = [];

    if (mode !== 'none') {
      // Helper: position a small label near an edge endpoint,
      // offset along the edge and slightly above/beside it.
      function _epPos(x, y, side, rank, count) {
        const along = 22, step = 12;
        // Multiple labels on same side: stagger symmetrically around centre
        const ct = count || 1, rk = (rank === undefined ? 0 : rank);
        const perp = ct > 1 ? (rk - (ct - 1) / 2) * step : -8;
        if (side === 'R') return {lx: x + along, ly: y + perp, anchor: 'start'};
        if (side === 'L') return {lx: x - along, ly: y + perp, anchor: 'end'};
        if (side === 'B') return {lx: x + perp,  ly: y + along, anchor: 'start'};
        /* T */           return {lx: x + perp,  ly: y - along, anchor: 'start'};
      }

      // Helper: add a single-line floating label (no background box for endpoint labels)
      function _addTxt(lx, ly, anchor, txt, color, fs) {
        const te = document.createElementNS(_NS, 'text');
        te.setAttribute('x', lx); te.setAttribute('y', ly);
        te.setAttribute('text-anchor', anchor);
        te.setAttribute('font-size', fs || 9);
        te.setAttribute('font-family', 'ui-sans-serif,system-ui,sans-serif');
        te.setAttribute('fill', color);
        te.style.pointerEvents = 'none';
        te.textContent = txt;
        _layer.appendChild(te); _pels[eid].textEls.push(te);
      }

      // Helper: add a boxed multi-line label at a midpoint
      function _addBoxedTxt(lx, ly, lines, color) {
        const fs = 9, lh = 11, pad = 3;
        const maxW = Math.max(...lines.map(l => l.length)) * 5.2;
        const bgH  = lines.length * lh + pad * 2;
        const bg = document.createElementNS(_NS, 'rect');
        bg.setAttribute('x', lx - maxW/2 - pad); bg.setAttribute('y', ly - bgH/2);
        bg.setAttribute('width', maxW + pad*2);   bg.setAttribute('height', bgH);
        bg.setAttribute('fill', '#f3f6fa');        bg.setAttribute('rx', 2);
        bg.style.pointerEvents = 'none';
        _layer.appendChild(bg); _pels[eid].textEls.push(bg);
        lines.forEach((ln, i) => {
          _addTxt(lx, ly - bgH/2 + pad + fs + i * lh, 'middle', ln, color, fs);
        });
      }

      // Midpoint helper (shared by several branches)
      function _midXY() {
        if (ep.sS === 'R' || ep.sS === 'L') {
          const mx = (ep.sS === ep.tS) ? (ep.sS === 'R' ? Math.max(sx,tx)+70 : Math.min(sx,tx)-70) : (sx+tx)/2;
          return [mx, (sy + ty) / 2];
        } else {
          const my = (ep.sS === ep.tS) ? (ep.sS === 'B' ? Math.max(sy,ty)+50 : Math.min(sy,ty)-50) : (sy+ty)/2;
          return [(sx + tx) / 2, my];
        }
      }

      if (d.kind === 'rel' || d.kind === 'rel_trans') {
        const parts = (d.metaLabel || '').split(' / ');
        const srcStrat = parts[0] || '', tgtStrat = parts[1] || '';
        const srcName  = e.source().data('label') || '?';
        const tgtName  = e.target().data('label') || '?';

        // Two endpoint labels (hidden in stats-only mode) — drawn as boxed pills
        if (mode !== 'stats') {
          function _epBoxedTxt(lx, ly, anchor, txt, color) {
            // _addBoxedTxt centers at lx; shift so box clears the node edge
            const hw = txt.length * 2.6 + 4;  // approx half-box width
            const cx = anchor === 'start' ? lx + hw : anchor === 'end' ? lx - hw : lx;
            _addBoxedTxt(cx, ly, [txt], color);
          }
          if (srcStrat) {
            const p = _epPos(sx, sy, ep.sS, ep.sR, ep.sC);
            _epBoxedTxt(p.lx, p.ly, p.anchor, srcStrat, st.color);
          }
          if (tgtStrat) {
            const p = _epPos(tx, ty, ep.tS, ep.tR, ep.tC);
            _epBoxedTxt(p.lx, p.ly, p.anchor, tgtStrat, st.color);
          }
        }

      } else if (d.kind === 'portal_edge' && !d.materialized) {
        // Cross-sdata sjoin suggestion portal: "sjoin" label at midpoint
        const [lx, ly] = _midXY();
        _addBoxedTxt(lx, ly, ['sjoin'], '#B05A00');
      } else if (d.kind !== 'portal_edge' || d.materialized) {
        // sjoin (within-sdata) and materialized portal: single centred label
        const ltext = edgeLabelText(e, d);
        if (ltext) {
          const lines = ltext.split('\n');
          const [lx, ly] = _midXY();
          _addBoxedTxt(lx, ly, lines, st.color);
        }
      }
    }
  });

  // Remove SVG elements for edges that no longer exist
  Object.keys(_pels).forEach(eid => {
    if (!seen.has(eid)) {
      const p = _pels[eid];
      p.glow.remove(); p.hit.remove(); p.line.remove(); p.textEls.forEach(el => el.remove());
      delete _pels[eid];
    }
  });

  _syncVP();
}

// ── store initial positions for "Reset layout" ────────────────────────────────
const _initPos = {};
cy.ready(() => {
  cy.nodes(':childless').forEach(n => { _initPos[n.id()] = {...n.position()}; });
  // Show transitive toggle whenever there are within-sdata transitive edges
  if (cy.edges('[kind="rel_trans"]').length > 0) {
    document.getElementById('datasets-section').style.display = '';
    document.getElementById('transitive-row').style.display   = '';
  }
  updateVisibility();
});

function resetLayout() {
  // 1. Restore initial positions
  cy.nodes(':childless').forEach(n => {
    if (_initPos[n.id()]) n.position({..._initPos[n.id()]});
  });

  // 2. Iterative bounding-box overlap removal for visible nodes
  const PAD      = 18;    // extra clearance padding around each node (px)
  const MAX_ITER = 150;
  const vis = cy.nodes(':visible').not('[kind="group"]');

  for (let iter = 0; iter < MAX_ITER; iter++) {
    const arr = vis.toArray();
    const pos = arr.map(n => ({...n.position()}));
    const hw  = arr.map(n => (n.data('nodeWidth')  || 120) / 2 + PAD);
    const hh  = arr.map(n => (n.data('nodeHeight') || 64)  / 2 + PAD);
    const dp  = arr.map(() => ({x: 0, y: 0}));
    let anyPush = false;

    for (let i = 0; i < arr.length; i++) {
      for (let j = i + 1; j < arr.length; j++) {
        const dx = pos[j].x - pos[i].x, dy = pos[j].y - pos[i].y;
        const ox = hw[i] + hw[j] - Math.abs(dx);
        const oy = hh[i] + hh[j] - Math.abs(dy);
        if (ox > 0 && oy > 0) {
          // Push apart along axis of smaller overlap (least disruption)
          if (ox <= oy) {
            const push = ox / 2 + 0.5;
            const sx = dx >= 0 ? push : -push;
            dp[i].x -= sx; dp[j].x += sx;
          } else {
            const push = oy / 2 + 0.5;
            const sy = dy >= 0 ? push : -push;
            dp[i].y -= sy; dp[j].y += sy;
          }
          anyPush = true;
        }
      }
    }

    arr.forEach((n, i) => {
      if (dp[i].x || dp[i].y)
        n.position({x: pos[i].x + dp[i].x, y: pos[i].y + dp[i].y});
    });

    if (!anyPush) break;
  }

  fitVisible();
  _renderEdges();
}

cy.on('render',   _syncVP);                  // cheap: just syncs SVG transform
cy.on('position drag', 'node', _renderEdges); // recompute paths when nodes move

// ── portal click → activate target sdata ─────────────────────────────────────
cy.on('tap', 'node[kind="portal"]', e => {
  const tgt = e.target.data('targetSdata');
  if (tgt) activateSdata(tgt);
});

// ── node interaction (hover / click → sidebar) ────────────────────────────────
cy.on('mouseover', 'node', e => {
  const n = e.target;
  if (n.data('kind') === 'group' || pinned) return;
  setInfo(nodeInfoHtml(n));
});
cy.on('mouseout', 'node', () => { if (!pinned) setInfo(EMPTY_INFO); });
cy.on('tap', 'node', e => {
  const n = e.target;
  if (n.data('kind') === 'group') return;
  pinned = true;
  setInfo(nodeInfoHtml(n));
});
cy.on('tap', e => {
  if (e.target === cy) { pinned = false; setInfo(EMPTY_INFO); }
});
</script>
</body>
</html>"""
