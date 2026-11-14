/* Browser dependency viewer: Cytoscape.js for rendering, ELK (layered) for layout.
 * Graph logic lives in graph.js (UmlGraph); this file is the UI shell. */
(function () {
  'use strict';
  const G = window.UmlGraph;
  const data = JSON.parse(document.getElementById('graph-data').textContent);
  const $ = (id) => document.getElementById(id);

  if (window.cytoscapeElk) cytoscape.use(window.cytoscapeElk);
  else if (typeof cytoscapeElk !== 'undefined') cytoscape.use(cytoscapeElk);

  // ---- model ---------------------------------------------------------------
  const tree = G.buildTree(data.nodes);
  const allEdges = data.edges.filter((e) => tree.byId.has(e.from) && tree.byId.has(e.to));
  const maxDepth = Math.max(...[...tree.byId.values()].map((t) => t.depth));
  const BUDGET = 70;          // visible nodes
  const DRAW_TARGET = 160;    // links drawn before 'auto' min-links starts thinning
  const EDGE_BUDGET = 260;    // rolled-up links; denser graphs start more collapsed
  const SMALL = 150;          // up to this many modules, open groups automatically
  // Big graphs start as the architecture overview (top-level groups); drill down on demand.
  const autoOpen = () => data.nodes.length <= SMALL
    ? G.autoExpand(tree, BUDGET, { edges: allEdges, edgeBudget: EDGE_BUDGET })
    : new Set();
  const leafCycles = G.cycles(data.nodes.map((n) => n.id), allEdges);
  const leafInCycle = new Set(leafCycles.flat());
  const totalViolations = allEdges.filter((e) => e.violating).length;
  const levelCount = Math.max(1, ...data.nodes.filter((n) => !n.foreign).map((n) => (n.level || 0) + 1));

  const state = {
    expanded: autoOpen(),
    depth: 'auto',
    hideForeign: false,
    onlyViolations: false,
    directOnly: false,
    simplify: true,         // hide links implied by a longer path
    minCount: 'auto',       // hide links backed by fewer deps ('auto' or a number)
    effMin: 1,
    hidden: new Set(), thin: 0, implied: 0,
    dir: 'DOWN',
    selected: null,         // {kind:'node'|'edge', id}
    graph: null,            // current visibleGraph
    comps: new Map(),       // node id -> visible-cycle component index
  };

  // ---- persisted UI prefs (best effort) ----------------------------------
  const KEY = 'uml-web:' + data.title;
  try {
    const p = JSON.parse(localStorage.getItem(KEY) || '{}');
    for (const k of ['hideForeign', 'onlyViolations', 'directOnly', 'simplify']) if (typeof p[k] === 'boolean') state[k] = p[k];
    if (p.minCount === 'auto' || [1, 2, 3, 5, 10].includes(p.minCount)) state.minCount = p.minCount;
    if (p.dir === 'DOWN' || p.dir === 'RIGHT') state.dir = p.dir;
  } catch (_) { /* storage unavailable */ }
  const persist = () => {
    try { localStorage.setItem(KEY, JSON.stringify({ hideForeign: state.hideForeign,
      onlyViolations: state.onlyViolations, directOnly: state.directOnly, simplify: state.simplify,
      minCount: state.minCount, dir: state.dir })); } catch (_) { /* ignore */ }
  };

  // ---- theme ----------------------------------------------------------------
  const dark = () => {
    const t = document.documentElement.getAttribute('data-theme');
    return t ? t === 'dark' : matchMedia('(prefers-color-scheme: dark)').matches;
  };
  const css = (name) => getComputedStyle(document.documentElement).getPropertyValue(name).trim();
  const HUES = [212, 152, 34, 285, 350, 182, 60, 255];
  const levelColor = (level, foreign) => {
    if (foreign) return dark() ? { fill: '#262d39', line: '#6b7689' } : { fill: '#eceef2', line: '#8a94a6' };
    const h = HUES[(level || 0) % HUES.length];
    // Cytoscape only parses comma-separated hsl(); the CSS4 space form falls back to grey.
    return dark() ? { fill: `hsl(${h}, 38%, 24%)`, line: `hsl(${h}, 55%, 62%)` }
                  : { fill: `hsl(${h}, 70%, 91%)`, line: `hsl(${h}, 45%, 42%)` };
  };
  const levelOf = (t) => {
    if (t.node && !t.isGroup) return t.node.level || 0;
    let best = Infinity;
    (function walk(x) { if (x.node && x.node.level !== undefined) best = Math.min(best, x.node.level || 0); x.children.forEach(walk); })(t);
    return best === Infinity ? 0 : best;
  };
  const isForeignGroup = (t) => { let all = true, any = false;
    (function walk(x) { if (x.node) { any = true; if (!x.node.foreign) all = false; } x.children.forEach(walk); })(t);
    return any && all; };

  // ---- cytoscape ---------------------------------------------------------------
  const cy = cytoscape({
    container: $('cy'), elements: [], wheelSensitivity: 0.25, minZoom: 0.03, maxZoom: 3,
    boxSelectionEnabled: false, autounselectify: true,
  });

  function buildStyle() {
    const ink = css('--ink'), edge = css('--edge'), bad = css('--bad'), cyc = css('--cycle'),
          out = css('--out'), inn = css('--in'), gline = css('--group-line');
    return [
      { selector: 'node', style: {
          label: 'data(label)', 'font-size': 12, color: ink, 'text-valign': 'center', 'text-halign': 'center',
          'text-wrap': 'none', shape: 'round-rectangle', width: 'data(w)', height: 34,
          'background-color': 'data(fill)', 'border-color': 'data(line)', 'border-width': 1.5,
          'font-family': 'ui-monospace, Menlo, Consolas, monospace' } },
      { selector: 'node[?foreign]', style: { 'border-style': 'dashed' } },
      { selector: 'node[?group][!expanded]', style: { 'border-width': 3, 'border-style': 'double' } },
      { selector: 'node[?expanded]', style: {
          'background-color': gline, 'background-opacity': 0.09, 'border-color': gline, 'border-width': 1.2, 'border-style': 'solid',
          'text-valign': 'top', 'text-halign': 'center', 'text-margin-y': -2, padding: 16, width: 'auto', height: 'auto',
          'font-size': 12, 'font-weight': 600, color: css('--muted') } },
      { selector: 'node[?cyc]', style: { 'border-color': cyc, 'border-width': 3 } },
      { selector: 'edge', style: {
          width: 'data(w)', 'line-color': edge, 'target-arrow-color': edge, 'target-arrow-shape': 'triangle',
          'arrow-scale': 0.9, 'curve-style': 'bezier', 'control-point-step-size': 36, opacity: 0.75, 'z-index': 1 } },
      { selector: 'edge[?implied]', style: { display: 'none' } },
      { selector: 'edge[?implied].revealed', style: { display: 'element', opacity: 0.55, 'line-style': 'dotted' } },
      { selector: 'edge[?cyc]', style: { 'line-color': cyc, 'target-arrow-color': cyc, 'line-style': 'dashed' } },
      { selector: 'edge[?violating]', style: { 'line-color': bad, 'target-arrow-color': bad, opacity: 1, 'z-index': 5 } },
      { selector: 'edge.out', style: { 'line-color': out, 'target-arrow-color': out, opacity: 1, 'z-index': 9 } },
      { selector: 'edge.in', style: { 'line-color': inn, 'target-arrow-color': inn, opacity: 1, 'z-index': 9 } },
      { selector: 'edge.sel', style: { opacity: 1, width: 5, 'z-index': 10 } },
      { selector: 'node.out', style: { 'border-color': out, 'border-width': 3.5 } },
      { selector: 'node.in', style: { 'border-color': inn, 'border-width': 3.5 } },
      { selector: 'node.sel', style: { 'border-color': ink, 'border-width': 4 } },
      { selector: 'node.match', style: { 'border-color': css('--accent'), 'border-width': 4 } },
      { selector: '.dim', style: { opacity: 0.12 } },
      { selector: 'node.dim[?expanded]', style: { opacity: 0.5 } },
      { selector: 'edge[count > 1]', style: { label: 'data(count)', 'font-size': 10, color: css('--muted'),
          'text-background-color': css('--bg'), 'text-background-opacity': 0.85, 'text-background-padding': 2 } },
    ];
  }

  // ---- render --------------------------------------------------------------------
  function elementsFor(g) {
    const ids = g.nodes.map((n) => n.id);
    // Violations are already red and would fuse most of the graph into one cycle: style cycles among conforming links only.
    const comps = G.cycles(ids, g.edges.filter((e) => !e.violating));
    state.comps = new Map();
    comps.forEach((c, i) => c.forEach((id) => state.comps.set(id, i)));
    const els = [];
    for (const v of g.nodes) {
      const t = tree.byId.get(v.id);
      const colors = levelColor(levelOf(t), isForeignGroup(t));
      const label = v.group && !v.expanded ? `${v.label}  ▸ ${v.leafCount}` : (v.expanded ? `${v.label}  ▾` : v.label);
      els.push({ group: 'nodes', data: {
        id: v.id, label, group: v.group, expanded: v.expanded, foreign: isForeignGroup(t),
        leafCount: v.leafCount, fill: colors.fill, line: colors.line, level: levelOf(t),
        w: Math.max(90, Math.round(label.length * 7.4 + 28)),
        cyc: state.comps.has(v.id) || (!v.group && leafInCycle.has(v.id)),
        parent: v.parent === null ? undefined : v.parent } });
    }
    for (const e of g.edges) {
      els.push({ group: 'edges', data: { id: e.id, source: e.from, target: e.to, count: e.count,
        violating: e.violating, implied: state.hidden.has(e.id), w: Math.min(7, 1.4 + Math.log2(e.count) * 1.1),
        cyc: state.comps.has(e.from) && state.comps.get(e.from) === state.comps.get(e.to) } });
    }
    return els;
  }

  let layoutToken = 0;
  function render({ fit = true } = {}) {
    state.graph = G.visibleGraph(tree, allEdges, state.expanded, { hideForeign: state.hideForeign });
    state.effMin = state.minCount === 'auto' ? G.autoMinCount(state.graph, DRAW_TARGET, state.simplify) : state.minCount;
    const simp = G.simplify(state.graph, { reduce: state.simplify, minCount: state.effMin });
    state.hidden = simp.hidden; state.thin = simp.thin; state.implied = simp.implied;
    const token = ++layoutToken;
    $('busy').hidden = false;
    const T = { start: performance.now() };
    const els = elementsFor(state.graph);
    T.elements = performance.now();
    cy.batch(() => { cy.elements().remove(); cy.add(els); });
    T.add = performance.now();
    cy.style(buildStyle());
    updateStats(); updateLegend();
    T.style = performance.now();
    const run = cy.layout({
      name: 'elk', fit, padding: 40, animate: false, nodeDimensionsIncludeLabels: true,
      eles: cy.elements().not('[?implied]'),
      // Bands by architecture level: outermost on top, innermost below it, external libs last.
      nodeLayoutOptions: (n) => {
        const lvl = n.data('foreign') ? levelCount : (levelCount - 1) - (n.data('level') || 0);
        return { 'elk.partitioning.partition': String(Math.max(0, lvl)), 'elk.partitioning.activate': 'true' };
      },
      elk: {
        algorithm: 'layered', 'elk.direction': state.dir,
        'elk.hierarchyHandling': 'INCLUDE_CHILDREN',
        'elk.partitioning.activate': 'true',
        'elk.layered.spacing.nodeNodeBetweenLayers': 70, 'elk.spacing.nodeNode': 26,
        'elk.spacing.componentComponent': 40,
        'elk.layered.crossingMinimization.strategy': 'LAYER_SWEEP',
        'elk.layered.nodePlacement.strategy': 'BRANDES_KOEPF',
        // NB: considerModelOrder=NODES_AND_EDGES made ELK ~40x slower on dense graphs; leave it off.
        'elk.layered.thoroughness': 3,
        'elk.padding': '[top=34,left=14,bottom=14,right=14]',
      },
    });
    run.promiseOn ? run.promiseOn('layoutstop').then(done) : run.one('layoutstop', done);
    run.run();
    function done() {
      if (token !== layoutToken) return;
      T.layout = performance.now();
      window.__umlTimings = { elementsMs: T.elements - T.start, addMs: T.add - T.elements, styleMs: T.style - T.add,
        layoutMs: T.layout - T.style, nodes: state.graph.nodes.length, edges: state.graph.edges.length };
      $('busy').hidden = true;
      applyFocus();
      window.__umlReady = true;
    }
  }

  // ---- focus / selection ----------------------------------------------------------
  function applyFocus(previewSel) {
    cy.elements().removeClass('dim out in sel match revealed');
    const sel = previewSel || state.selected;
    const dimAll = () => cy.elements().addClass('dim');
    if (state.onlyViolations) {
      const touched = new Set();
      state.graph.edges.forEach((e) => { if (e.violating) { touched.add(e.from); touched.add(e.to); } });
      cy.edges().filter((e) => !e.data('violating')).addClass('dim');
      cy.nodes().filter((n) => !touched.has(n.id()) && !n.data('expanded')).addClass('dim');
    }
    if (!sel) return;
    if (sel.kind === 'edge') {
      const e = cy.getElementById(sel.id);
      if (e.empty()) return;
      dimAll(); e.removeClass('dim').addClass('sel');
      e.source().removeClass('dim').addClass('out'); e.target().removeClass('dim').addClass('in');
      return;
    }
    const n = cy.getElementById(sel.id);
    if (n.empty()) return;
    dimAll();
    let direct = state.directOnly;
    state.autoDirect = false;
    if (!direct) {
      const o = G.reach(state.graph.edges, sel.id, 'out'), i = G.reach(state.graph.edges, sel.id, 'in');
      // When the trace would light up most of the graph it says nothing: show direct links.
      if (new Set([...o, ...i]).size > 0.6 * state.graph.nodes.length) { direct = true; state.autoDirect = true; }
    }
    const outSet = direct ? new Set(state.graph.edges.filter((e) => e.from === sel.id).map((e) => e.to)) : G.reach(state.graph.edges, sel.id, 'out');
    const inSet = direct ? new Set(state.graph.edges.filter((e) => e.to === sel.id).map((e) => e.from)) : G.reach(state.graph.edges, sel.id, 'in');
    n.removeClass('dim').addClass('sel');
    cy.nodes().forEach((x) => {
      if (outSet.has(x.id())) x.removeClass('dim').addClass('out');
      if (inSet.has(x.id())) x.removeClass('dim').addClass(outSet.has(x.id()) ? 'out' : 'in');
    });
    cy.edges().forEach((e) => {
      const a = e.source().id(), b = e.target().id();
      // Direct mode lights only links touching the selection; transitive mode lights whole paths.
      const fromSel = direct ? a === sel.id : (a === sel.id || outSet.has(a)), toOut = outSet.has(b);
      const toSel = direct ? b === sel.id : (b === sel.id || inSet.has(b)), fromIn = inSet.has(a);
      if (fromSel && toOut) e.removeClass('dim').addClass('out');
      else if (toSel && fromIn) e.removeClass('dim').addClass('in');
      if (e.data('implied') && (a === sel.id || b === sel.id)) e.addClass('revealed');
    });
    // keep containers readable when any child is lit
    cy.nodes('[?expanded]').forEach((p) => { if (p.descendants().some((d) => !d.hasClass('dim'))) p.removeClass('dim'); });
  }

  function select(sel) {
    state.selected = sel;
    applyFocus(); renderDetail();
  }

  // ---- panels ---------------------------------------------------------------------
  function updateStats() {
    const g = state.graph;
    const vis = g.edges.filter((e) => e.violating).length;
    const leaves = data.nodes.filter((n) => !n.foreign).length;
    $('stats').innerHTML =
      `<b>${leaves}</b> modules · <b>${allEdges.length}</b> deps · showing <b>${g.nodes.length}</b> nodes / <b>${g.edges.length}</b> links` +
      (totalViolations ? ` · <span class="bad">${totalViolations} violation${totalViolations > 1 ? 's' : ''}${vis ? '' : ' (hidden in collapsed groups)'}</span>` : ' · 0 violations') +
      (state.hidden.size ? ` · <span title="${state.implied} implied by a longer path, ${state.thin} backed by fewer than ${state.effMin} deps. Select a node to see its hidden links.">${state.hidden.size} links hidden${state.minCount === 'auto' && state.effMin > 1 ? ` (min ${state.effMin})` : ''}</span>` : '') +
      (leafCycles.length ? ` · <span class="cyc" title="Real dependency cycles between modules">${leafCycles.length} cycle${leafCycles.length > 1 ? 's' : ''}</span>` : ' · 0 cycles');
  }

  function updateLegend() {
    const rows = [];
    const lv = data.levels || [];
    for (let i = 0; i < levelCount; i++) {
      const c = levelColor(i, false);
      const name = lv[i] ? (Array.isArray(lv[i]) ? lv[i].join(' / ') : String(lv[i])) : 'level ' + i;
      rows.push(`<div class="legend-row"><span class="sw" style="background:${c.fill};border-color:${c.line}"></span>${esc(name)}${i === 0 ? ' (innermost)' : ''}</div>`);
    }
    const f = levelColor(0, true);
    rows.push(`<div class="legend-row"><span class="sw" style="background:${f.fill};border-color:${f.line};border-style:dashed"></span>external</div>`);
    rows.push('<div class="legend-row"><span class="ln"></span>depends on · label = count</div>');
    rows.push('<div class="legend-row"><span class="ln bad"></span>violates dependency rule</div>');
    rows.push('<div class="legend-row"><span class="ln cyc"></span>in a dependency cycle</div>');
    rows.push('<div class="legend-row"><span class="ln out"></span>selected → depends on</div>');
    rows.push('<div class="legend-row"><span class="ln in"></span>used by → selected</div>');
    $('legend').innerHTML = rows.join('');
  }

  const esc = (s) => String(s).replace(/[&<>"]/g, (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;' }[c]));
  const linkList = (items) => items.length
    ? `<ul class="links">${items.map((i) => `<li data-id="${esc(i.id)}" class="${i.violating ? 'bad' : ''}"><span>${esc(i.id)}</span><span class="n">${i.count > 1 ? '×' + i.count : ''}</span></li>`).join('')}</ul>`
    : '<p class="hint">none</p>';

  function renderDetail() {
    const d = $('detail');
    const sel = state.selected;
    if (!sel) { d.innerHTML = '<p class="hint">Click a module to trace what it depends on and what depends on it. Double‑click a group to open or close it.</p>'; return; }
    if (sel.kind === 'edge') {
      const e = state.graph.edges.find((x) => x.id === sel.id);
      if (!e) { d.innerHTML = ''; return; }
      d.innerHTML = `<h2>${esc(e.from)}<br>→ ${esc(e.to)}</h2>` +
        `<p>${e.violating ? '<span class="pill bad">violates dependency rule</span>' : ''}<span class="pill">${e.count} dependenc${e.count > 1 ? 'ies' : 'y'}</span></p>` +
        '<h3>Underlying dependencies</h3><ul class="links">' +
        e.members.slice(0, 200).map((m) => `<li class="${m.violating ? 'bad' : ''}" data-id="${esc(m.from)}"><span>${esc(m.from)}<br>→ ${esc(m.to)}</span></li>`).join('') +
        (e.members.length > 200 ? `<li><span>… ${e.members.length - 200} more</span></li>` : '') + '</ul>';
      return;
    }
    const v = state.graph.nodes.find((x) => x.id === sel.id);
    const t = tree.byId.get(sel.id);
    if (!v || !t) { d.innerHTML = ''; return; }
    const outs = state.graph.edges.filter((e) => e.from === sel.id).map((e) => ({ id: e.to, count: e.count, violating: e.violating }));
    const ins = state.graph.edges.filter((e) => e.to === sel.id).map((e) => ({ id: e.from, count: e.count, violating: e.violating }));
    const byCount = (a, b) => b.count - a.count || (a.id < b.id ? -1 : 1);
    outs.sort(byCount); ins.sort(byCount);
    d.innerHTML = `<h2>${esc(sel.id)}</h2><p>` +
      (v.group ? `<span class="pill">group · ${v.leafCount} module${v.leafCount > 1 ? 's' : ''}</span>` : '') +
      (t.node && t.node.foreign ? '<span class="pill">external</span>' : '') +
      (state.comps.has(sel.id) || leafInCycle.has(sel.id) ? '<span class="pill cyc">in a cycle</span>' : '') +
      (outs.some((o) => o.violating) ? '<span class="pill bad">has violations</span>' : '') + '</p>' +
      (t.node && t.node.file ? `<p class="kv">${esc(t.node.file)}</p>` : '') +
      (state.autoDirect ? '<p class="hint">Transitive trace covers most of the graph, so only direct links are highlighted.</p>' : '') +
      `<h3>Depends on (${outs.length})</h3>${linkList(outs)}<h3>Used by (${ins.length})</h3>${linkList(ins)}`;
  }

  $('detail').addEventListener('click', (ev) => {
    const li = ev.target.closest('li[data-id]');
    if (li) focusNode(li.dataset.id);
  });

  // ---- navigation helpers ----------------------------------------------------------
  function focusNode(id) {
    const t = tree.byId.get(id);
    if (!t) return;
    if (t.node && t.node.foreign && state.hideForeign) { state.hideForeign = false; $('hide-foreign').checked = false; persist(); }
    let target = id;
    const rep = G.representative(tree, id, state.expanded);
    const needReveal = rep !== id && !state.expanded.has(G.parentOf(id) || '');
    if (needReveal) { state.expanded = G.revealing(state.expanded, id); state.depth = 'custom'; syncDepth(); }
    const after = () => { select({ kind: 'node', id: target }); const n = cy.getElementById(target);
      if (n.nonempty()) cy.animate({ center: { eles: n }, zoom: Math.max(cy.zoom(), 0.9) }, { duration: 250 }); };
    if (needReveal) { render({ fit: false }); waitReady(after); } else after();
  }
  function waitReady(fn) { const tick = () => ($('busy').hidden ? fn() : setTimeout(tick, 30)); setTimeout(tick, 30); }

  function toggleGroup(id) {
    if (state.expanded.has(id)) {
      // collapsing also forgets open descendants so reopening is predictable
      for (const x of [...state.expanded]) if (x === id || x.startsWith(id + '.')) state.expanded.delete(x);
    } else state.expanded.add(id);
    state.depth = 'custom'; syncDepth();
    state.selected = null; renderDetail();   // the first tap of the double-tap selected it; a trace would dim the new children
    const opened = state.expanded.has(id);
    render({ fit: false });
    waitReady(() => {
      const n = cy.getElementById(id);
      // Opening a group: zoom to it. Closing: back to the overview.
      if (opened && n.nonempty()) cy.animate({ fit: { eles: n.union(n.descendants()), padding: 50 } }, { duration: 250 });
      else cy.animate({ fit: { padding: 40 } }, { duration: 250 });
    });
  }

  // Hovering previews the trace without committing a selection.
  cy.on('mouseover', 'node', (ev) => { if (!state.selected && !ev.target.data('expanded')) applyFocus({ kind: 'node', id: ev.target.id() }); });
  cy.on('mouseout', 'node', () => { if (!state.selected) applyFocus(); });
  // Cytoscape has no dbltap event: detect two taps on the same node ourselves.
  let lastTap = { id: null, t: 0 };
  cy.on('tap', 'node', (ev) => {
    const id = ev.target.id(), now = Date.now();
    const dbl = lastTap.id === id && now - lastTap.t < 350;
    lastTap = { id: dbl ? null : id, t: now };
    if (dbl && ev.target.data('group')) { toggleGroup(id); return; }
    select({ kind: 'node', id });
  });
  cy.on('tap', 'edge', (ev) => select({ kind: 'edge', id: ev.target.id() }));
  cy.on('tap', (ev) => { if (ev.target === cy) select(null); });

  // ---- toolbar ----------------------------------------------------------------------
  function syncDepth() { $('depth').value = state.depth; }
  const depthSel = $('depth');
  depthSel.innerHTML = '<option value="auto">auto</option>' +
    Array.from({ length: maxDepth }, (_, i) => `<option value="${i + 1}">${i + 1}</option>`).join('') +
    '<option value="all">all</option><option value="custom" hidden>custom</option>';
  depthSel.addEventListener('change', () => {
    const v = depthSel.value;
    state.depth = v;
    state.expanded = v === 'auto' ? autoOpen() : v === 'all' ? G.allGroups(tree) : G.expandToDepth(tree, +v);
    render();
  });
  $('expand-all').onclick = () => { state.depth = 'all'; syncDepth(); state.expanded = G.allGroups(tree); render(); };
  $('collapse-all').onclick = () => { state.depth = 1; syncDepth(); state.expanded = new Set(); render(); };
  const bindCheck = (id, key, rerender) => {
    const el = $(id); el.checked = state[key];
    el.addEventListener('change', () => { state[key] = el.checked; persist(); rerender(); });
  };
  bindCheck('hide-foreign', 'hideForeign', () => render());
  bindCheck('only-violations', 'onlyViolations', () => applyFocus());
  bindCheck('direct-only', 'directOnly', () => applyFocus());
  bindCheck('simplify', 'simplify', () => render({ fit: false }));
  const minSel = $('min-count'); minSel.value = String(state.minCount);
  minSel.addEventListener('change', () => { state.minCount = minSel.value === 'auto' ? 'auto' : +minSel.value; persist(); render({ fit: false }); });
  const dirBtn = $('dir');
  const syncDir = () => { dirBtn.textContent = state.dir === 'DOWN' ? '↓ Top‑down' : '→ Left‑right'; };
  syncDir();
  dirBtn.onclick = () => { state.dir = state.dir === 'DOWN' ? 'RIGHT' : 'DOWN'; syncDir(); persist(); render(); };
  $('fit').onclick = () => cy.fit(undefined, 40);
  $('png').onclick = () => {
    const a = document.createElement('a');
    a.href = cy.png({ full: true, scale: 2, bg: css('--bg') });
    a.download = (data.title || 'dependencies').replace(/\W+/g, '-').toLowerCase() + '.png';
    a.click();
  };

  // ---- search ------------------------------------------------------------------------
  const input = $('search'), results = $('results');
  let hits = [], cursor = -1;
  function paintResults() {
    results.hidden = hits.length === 0;
    results.innerHTML = hits.map((id, i) => `<li role="option" data-id="${esc(id)}" aria-selected="${i === cursor}">${esc(id)}</li>`).join('');
  }
  input.addEventListener('input', () => {
    hits = G.search(tree, input.value).sort((a, b) => a.length - b.length || (a < b ? -1 : 1)).slice(0, 14);
    cursor = hits.length ? 0 : -1; paintResults();
  });
  input.addEventListener('keydown', (ev) => {
    if (ev.key === 'ArrowDown') { cursor = Math.min(hits.length - 1, cursor + 1); paintResults(); ev.preventDefault(); }
    else if (ev.key === 'ArrowUp') { cursor = Math.max(0, cursor - 1); paintResults(); ev.preventDefault(); }
    else if (ev.key === 'Enter' && hits[cursor]) { pick(hits[cursor]); }
    else if (ev.key === 'Escape') { input.value = ''; hits = []; paintResults(); input.blur(); }
  });
  results.addEventListener('mousedown', (ev) => { const li = ev.target.closest('li'); if (li) pick(li.dataset.id); });
  function pick(id) { hits = []; paintResults(); input.blur(); focusNode(id); }
  document.addEventListener('click', (ev) => { if (!ev.target.closest('#search-wrap')) { hits = []; paintResults(); } });

  document.addEventListener('keydown', (ev) => {
    if (ev.target.matches('input, select, textarea')) return;
    if (ev.key === '/') { input.focus(); ev.preventDefault(); }
    else if (ev.key === 'f') cy.fit(undefined, 40);
    else if (ev.key === 'Escape') select(null);
  });

  matchMedia('(prefers-color-scheme: dark)').addEventListener('change', () => render({ fit: false }));
  window.addEventListener('resize', () => cy.resize());

  $('title').textContent = data.title || 'Dependencies';
  syncDepth();
  render();
  window.__uml = { cy, state, tree };
})();
