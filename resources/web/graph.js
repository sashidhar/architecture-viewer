/* Pure dependency-graph logic for the browser viewer. No DOM, no Cytoscape.
 * Loaded as a global (UmlGraph) in the page and via require() in node tests.
 *
 * Model
 *   nodes: [{id, name, level, foreign, file, ns}]   id is dotted: "domain.isr.append"
 *   edges: [{from, to, kind, violating}]            between leaf node ids
 *
 * The dotted ids form a tree. A group is any dotted prefix that has
 * descendants. Collapsing a group replaces its subtree by one node and rolls
 * the edges up onto it, which keeps a graph with thousands of edges readable.
 */
(function (root, factory) {
  if (typeof module === 'object' && module.exports) module.exports = factory();
  else root.UmlGraph = factory();
})(typeof self !== 'undefined' ? self : this, function () {
  'use strict';

  const segs = (id) => id.split('.');
  const parentOf = (id) => {
    const i = id.lastIndexOf('.');
    return i < 0 ? null : id.slice(0, i);
  };

  /** Build the namespace tree. Returns {byId, roots, leaves}. */
  function buildTree(nodes) {
    const byId = new Map();
    const ensure = (id) => {
      let t = byId.get(id);
      if (!t) {
        t = { id, name: segs(id).pop(), parent: parentOf(id), children: [],
              node: null, depth: segs(id).length };
        byId.set(id, t);
        if (t.parent !== null) ensure(t.parent).children.push(t);
      }
      return t;
    };
    for (const n of nodes) ensure(n.id).node = n;
    const roots = [...byId.values()].filter((t) => t.parent === null);
    const sortKids = (t) => { t.children.sort((a, b) => a.id < b.id ? -1 : 1); t.children.forEach(sortKids); };
    roots.sort((a, b) => a.id < b.id ? -1 : 1).forEach(sortKids);
    for (const t of byId.values()) {
      t.isGroup = t.children.length > 0;
      t.leafCount = 0;
    }
    const count = (t) => {
      t.leafCount = (t.node ? 1 : 0) + t.children.reduce((s, c) => s + count(c), 0);
      return t.leafCount;
    };
    roots.forEach(count);
    return { byId, roots };
  }

  /** Visible representative of `id`: the highest collapsed ancestor, else itself. */
  function representative(tree, id, expanded) {
    const parts = segs(id);
    for (let n = 1; n < parts.length; n++) {
      const anc = parts.slice(0, n).join('.');
      const t = tree.byId.get(anc);
      if (t && t.isGroup && !expanded.has(anc)) return anc;
    }
    return id;
  }

  /**
   * Graph for the current expansion state.
   * nodes: visible tree nodes ({id, label, group, expanded, parent, leafCount, ...})
   * edges: aggregated ({id, from, to, count, violating, members:[{from,to}]})
   */
  function visibleGraph(tree, edges, expanded, opts) {
    opts = opts || {};
    const hideForeign = !!opts.hideForeign;
    const isForeign = (id) => { const t = tree.byId.get(id); return !!(t && t.node && t.node.foreign); };

    const vis = new Map();
    const visit = (t) => {
      if (hideForeign && !t.isGroup && t.node && t.node.foreign) return;
      const open = t.isGroup && expanded.has(t.id);
      vis.set(t.id, { id: t.id, label: t.name, group: t.isGroup, expanded: open,
                      parent: t.parent, leafCount: t.leafCount, node: t.node });
      if (open) t.children.forEach(visit);
    };
    tree.roots.forEach(visit);

    // A visible node is parented only if its parent is itself visible & open.
    for (const v of vis.values()) if (v.parent !== null && !vis.has(v.parent)) v.parent = null;

    const agg = new Map();
    let internal = 0;
    for (const e of edges) {
      if (hideForeign && (isForeign(e.from) || isForeign(e.to))) continue;
      const a = representative(tree, e.from, expanded);
      const b = representative(tree, e.to, expanded);
      if (!vis.has(a) || !vis.has(b)) continue;
      if (a === b) { internal++; continue; }
      // An edge into/out of an open group's own subtree adds no information.
      const key = a + '\u0000' + b;
      let g = agg.get(key);
      if (!g) { g = { id: 'e:' + key, from: a, to: b, count: 0, violating: false, members: [] }; agg.set(key, g); }
      g.count++;
      if (e.violating) g.violating = true;
      g.members.push({ from: e.from, to: e.to, violating: !!e.violating });
    }
    return { nodes: [...vis.values()], edges: [...agg.values()], internalEdges: internal };
  }

  /** Strongly connected components (Tarjan, iterative). Returns array of id arrays, size>1 only. */
  function cycles(nodeIds, edges) {
    const adj = new Map(nodeIds.map((id) => [id, []]));
    for (const e of edges) if (adj.has(e.from) && adj.has(e.to)) adj.get(e.from).push(e.to);
    let idx = 0;
    const index = new Map(), low = new Map(), on = new Set(), stack = [], out = [];
    for (const start of nodeIds) {
      if (index.has(start)) continue;
      const work = [[start, 0]];
      index.set(start, idx); low.set(start, idx); idx++; stack.push(start); on.add(start);
      while (work.length) {
        const frame = work[work.length - 1];
        const [v, i] = frame;
        const nbrs = adj.get(v);
        if (i < nbrs.length) {
          frame[1]++;
          const w = nbrs[i];
          if (!index.has(w)) {
            index.set(w, idx); low.set(w, idx); idx++; stack.push(w); on.add(w);
            work.push([w, 0]);
          } else if (on.has(w)) {
            low.set(v, Math.min(low.get(v), index.get(w)));
          }
        } else {
          if (low.get(v) === index.get(v)) {
            const comp = [];
            let w;
            do { w = stack.pop(); on.delete(w); comp.push(w); } while (w !== v);
            if (comp.length > 1) out.push(comp);
          }
          work.pop();
          if (work.length) {
            const p = work[work.length - 1][0];
            low.set(p, Math.min(low.get(p), low.get(v)));
          }
        }
      }
    }
    return out;
  }

  /** Transitive reach from `id` following `dir` ('out' = depends on, 'in' = used by). */
  function reach(edges, id, dir) {
    const next = new Map();
    for (const e of edges) {
      const [a, b] = dir === 'out' ? [e.from, e.to] : [e.to, e.from];
      if (!next.has(a)) next.set(a, []);
      next.get(a).push(b);
    }
    const seen = new Set();
    const q = [id];
    while (q.length) {
      const v = q.pop();
      for (const w of next.get(v) || []) if (!seen.has(w) && w !== id) { seen.add(w); q.push(w); }
    }
    return seen;
  }

  /**
   * Initial expansion: open groups biggest-first until opening one more would
   * push the visible node count past `budget` or, when `opts.edges` is given,
   * the rolled-up edge count past `opts.edgeBudget`. Dense graphs therefore
   * start more collapsed, which keeps the first layout fast and legible.
   */
  function autoExpand(tree, budget, opts) {
    opts = opts || {};
    const expanded = new Set();
    let visible = tree.roots.length;
    const edgeCount = (set) => G_visibleEdgeCount(tree, opts.edges, set);
    const queue = [...tree.roots].filter((t) => t.isGroup);
    while (queue.length) {
      queue.sort((a, b) => b.leafCount - a.leafCount);
      const t = queue.shift();
      const grow = t.children.length; // the open group stays visible as a container
      if (visible + grow > budget) continue;
      if (opts.edges && opts.edgeBudget) {
        expanded.add(t.id);
        const over = edgeCount(expanded) > opts.edgeBudget;
        expanded.delete(t.id);
        if (over) continue;
      }
      expanded.add(t.id);
      visible += grow;
      t.children.filter((c) => c.isGroup).forEach((c) => queue.push(c));
    }
    return expanded;
  }

  function G_visibleEdgeCount(tree, edges, expanded) {
    const seen = new Set();
    for (const e of edges) {
      const a = representative(tree, e.from, expanded), b = representative(tree, e.to, expanded);
      if (a !== b) seen.add(a + '\u0000' + b);
    }
    return seen.size;
  }

  /**
   * Which aggregated edges to draw.
   *  - minCount: hide thin links (fewer than N underlying deps); violations always stay.
   *  - reduce:   transitive reduction over the condensation: hide A->C when A->B->C
   *              exists. Edges inside a cycle and violating edges always stay.
   * Returns {hidden: Set<edgeId>, thin, implied}. Nothing is removed from the model.
   */
  function simplify(graph, opts) {
    opts = opts || {};
    const minCount = opts.minCount || 1;
    const hidden = new Set();
    let thin = 0, implied = 0;
    const live = [];
    for (const e of graph.edges) {
      if (!e.violating && e.count < minCount) { hidden.add(e.id); thin++; } else live.push(e);
    }
    if (opts.reduce) {
      // Violations are always drawn, so they must not glue everything into one
      // cycle and block the reduction: reduce over the conforming edges only.
      const conforming = live.filter((e) => !e.violating);
      const comps = cycles(graph.nodes.map((n) => n.id), conforming);
      const cid = new Map();
      comps.forEach((c, i) => c.forEach((id) => cid.set(id, 'c' + i)));
      const rep = (id) => cid.get(id) || id;
      const adj = new Map();
      for (const e of conforming) {
        const a = rep(e.from), b = rep(e.to);
        if (a === b) continue;
        if (!adj.has(a)) adj.set(a, new Set());
        adj.get(a).add(b);
      }
      const memo = new Map();
      const reachable = (u) => {            // condensation is a DAG, so this terminates
        if (memo.has(u)) return memo.get(u);
        const out = new Set();
        memo.set(u, out);
        for (const v of adj.get(u) || []) { out.add(v); for (const w of reachable(v)) out.add(w); }
        return out;
      };
      for (const e of live) {
        const a = rep(e.from), b = rep(e.to);
        if (a === b || e.violating) continue;
        for (const m of adj.get(a) || []) {
          if (m !== b && reachable(m).has(b)) { hidden.add(e.id); implied++; break; }
        }
      }
    }
    return { hidden, thin, implied };
  }

  /**
   * Smallest "min links" threshold that leaves at most `target` drawn links
   * (after reduction). Violations never count against the target: they are
   * always drawn. Returns 1 when the graph is already sparse enough.
   */
  function autoMinCount(graph, target, reduce) {
    for (const m of [1, 2, 3, 4, 6, 8, 12, 16, 24, 32, 48, 64, 96, 128]) {
      const r = simplify(graph, { minCount: m, reduce });
      const drawn = graph.edges.filter((e) => !r.hidden.has(e.id) && !e.violating).length;
      if (drawn <= target) return m;
    }
    return 128;
  }

  function expandToDepth(tree, depth) {
    const s = new Set();
    for (const t of tree.byId.values()) if (t.isGroup && t.depth < depth) s.add(t.id);
    return s;
  }

  function allGroups(tree) {
    return new Set([...tree.byId.values()].filter((t) => t.isGroup).map((t) => t.id));
  }

  /** Substring match on id; returns ids of tree nodes (leaf or group). */
  function search(tree, q) {
    q = q.trim().toLowerCase();
    if (!q) return [];
    return [...tree.byId.keys()].filter((id) => id.toLowerCase().includes(q));
  }

  /** Expansion state that makes `id` visible (opens every ancestor group). */
  function revealing(expanded, id) {
    const s = new Set(expanded);
    for (let p = parentOf(id); p !== null; p = parentOf(p)) s.add(p);
    return s;
  }

  return { buildTree, visibleGraph, representative, cycles, reach, simplify, autoMinCount,
           autoExpand, expandToDepth, allGroups, search, revealing, parentOf };
});
