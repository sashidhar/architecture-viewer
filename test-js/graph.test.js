'use strict';
const test = require('node:test');
const assert = require('node:assert/strict');
const G = require('../resources/web/graph.js');

const n = (id, extra) => Object.assign({ id, name: id, level: 0 }, extra);
const nodes = [
  n('domain.isr.append'), n('domain.isr.spec'), n('domain.isr.telemetry'),
  n('domain.model.user'), n('internal.codec'),
  n('taoensso', { foreign: true }),
];
const e = (from, to, violating) => ({ from, to, kind: 'dependency', violating: !!violating });
const edges = [
  e('domain.isr.append', 'domain.isr.spec'),
  e('domain.isr.telemetry', 'domain.isr.spec'),
  e('domain.isr.telemetry', 'domain.isr.append'),
  e('domain.isr.append', 'domain.model.user', true),
  e('internal.codec', 'domain.isr.spec'),
  e('internal.codec', 'taoensso'),
];

test('buildTree makes groups of dotted prefixes with leaf counts', () => {
  const t = G.buildTree(nodes);
  assert.deepEqual(t.roots.map((r) => r.id), ['domain', 'internal', 'taoensso']);
  assert.equal(t.byId.get('domain').leafCount, 4);
  assert.equal(t.byId.get('domain.isr').isGroup, true);
  assert.equal(t.byId.get('domain.isr.append').isGroup, false);
});

test('fully collapsed: edges roll up to top-level groups, intra-group edges drop', () => {
  const t = G.buildTree(nodes);
  const g = G.visibleGraph(t, edges, new Set());
  assert.deepEqual(g.nodes.map((x) => x.id).sort(), ['domain', 'internal', 'taoensso']);
  const byPair = Object.fromEntries(g.edges.map((x) => [x.from + '>' + x.to, x]));
  assert.equal(byPair['internal>domain'].count, 1);
  assert.equal(byPair['internal>taoensso'].count, 1);
  assert.equal(g.edges.length, 2);
  assert.equal(g.internalEdges, 4);
});

test('a rolled-up edge is violating if any member is', () => {
  const t = G.buildTree(nodes);
  const open = new Set(['domain']);
  const g = G.visibleGraph(t, edges, open);
  const x = g.edges.find((q) => q.from === 'domain.isr' && q.to === 'domain.model');
  assert.equal(x.violating, true);
  assert.equal(x.members.length, 1);
});

test('expanded group appears as parent of its visible children', () => {
  const t = G.buildTree(nodes);
  const g = G.visibleGraph(t, edges, new Set(['domain', 'domain.isr']));
  const append = g.nodes.find((x) => x.id === 'domain.isr.append');
  assert.equal(append.parent, 'domain.isr');
  assert.equal(g.nodes.find((x) => x.id === 'domain').expanded, true);
  assert.equal(g.nodes.find((x) => x.id === 'domain.model').group, true);
});

test('hideForeign removes foreign nodes and their edges', () => {
  const t = G.buildTree(nodes);
  const g = G.visibleGraph(t, edges, new Set(), { hideForeign: true });
  assert.equal(g.nodes.some((x) => x.id === 'taoensso'), false);
  assert.equal(g.edges.some((x) => x.to === 'taoensso'), false);
});

test('cycles finds strongly connected components only', () => {
  const ids = ['a', 'b', 'c', 'd'];
  const es = [e('a', 'b'), e('b', 'c'), e('c', 'a'), e('c', 'd')];
  const cs = G.cycles(ids, es);
  assert.equal(cs.length, 1);
  assert.deepEqual(cs[0].sort(), ['a', 'b', 'c']);
  assert.deepEqual(G.cycles(['a', 'b'], [e('a', 'b')]), []);
});

test('cycles handles a long chain without blowing the stack', () => {
  const ids = Array.from({ length: 50000 }, (_, i) => 'n' + i);
  const es = ids.slice(1).map((id, i) => e(ids[i], id));
  es.push(e('n49999', 'n0'));
  const cs = G.cycles(ids, es);
  assert.equal(cs.length, 1);
  assert.equal(cs[0].length, 50000);
});

test('reach follows direction transitively', () => {
  const es = [e('a', 'b'), e('b', 'c'), e('x', 'c')];
  assert.deepEqual([...G.reach(es, 'a', 'out')].sort(), ['b', 'c']);
  assert.deepEqual([...G.reach(es, 'c', 'in')].sort(), ['a', 'b', 'x']);
});

test('autoExpand opens the biggest groups first and respects the budget', () => {
  const t = G.buildTree(nodes);
  assert.equal(G.autoExpand(t, 4).has('domain'), false); // 3 roots + 2 children = 5 > 4
  const ex = G.autoExpand(t, 5);
  assert.equal(ex.has('domain'), true);
  const vis = G.visibleGraph(t, edges, ex).nodes.length;
  assert.ok(vis <= 5, 'visible ' + vis);
  assert.equal(G.autoExpand(t, 100).has('domain.isr'), true);
});

test('revealing opens every ancestor; search is case-insensitive substring', () => {
  const ex = G.revealing(new Set(), 'domain.isr.append');
  assert.deepEqual([...ex].sort(), ['domain', 'domain.isr']);
  const t = G.buildTree(nodes);
  assert.ok(G.search(t, 'APPEND').includes('domain.isr.append'));
  assert.deepEqual(G.search(t, '  '), []);
});

test('scales: 3000 leaves / 12000 edges aggregate quickly', () => {
  const big = [];
  for (let a = 0; a < 20; a++) for (let b = 0; b < 15; b++) for (let c = 0; c < 10; c++) big.push(n(`l${a}.m${b}.c${c}`));
  const es = [];
  let seed = 1; const rnd = () => (seed = (seed * 1103515245 + 12345) & 0x7fffffff) / 0x7fffffff;
  for (let i = 0; i < 12000; i++) es.push(e(big[(rnd() * big.length) | 0].id, big[(rnd() * big.length) | 0].id));
  const t = G.buildTree(big);
  const t0 = Date.now();
  const ex = G.autoExpand(t, 60);
  const g = G.visibleGraph(t, es, ex);
  assert.ok(g.nodes.length <= 60);
  assert.ok(Date.now() - t0 < 1500);
});

test('autoExpand respects an edge budget on dense graphs', () => {
  const t = G.buildTree(nodes);
  const free = G.autoExpand(t, 100, { edges, edgeBudget: 1000 });
  assert.equal(free.has('domain.isr'), true);
  const tight = G.autoExpand(t, 100, { edges, edgeBudget: 1 });
  assert.equal(tight.has('domain'), false);
});

test('simplify hides thin links but never violations', () => {
  const t = G.buildTree(nodes);
  const g = G.visibleGraph(t, edges, new Set(['domain', 'domain.isr']));
  const r = G.simplify(g, { minCount: 2 });
  const viol = g.edges.find((x) => x.violating);
  assert.equal(r.hidden.has(viol.id), false);
  assert.ok(r.thin > 0);
});

test('simplify reduce drops implied edges, keeps cycle and violating edges', () => {
  const ns = ['a', 'b', 'c', 'x', 'y'].map((id) => n(id));
  const t = G.buildTree(ns);
  const es = [e('a', 'b'), e('b', 'c'), e('a', 'c'),          // a->c implied by a->b->c
              e('x', 'y'), e('y', 'x'),                        // a cycle stays
              e('a', 'x', true), e('x', 'c'), e('a', 'c')];    // violating a->x kept
  const g = G.visibleGraph(t, es, new Set());
  const r = G.simplify(g, { reduce: true });
  const id = (a, b) => 'e:' + a + '\u0000' + b;
  assert.equal(r.hidden.has(id('a', 'c')), true);
  assert.equal(r.hidden.has(id('a', 'b')), false);
  assert.equal(r.hidden.has(id('x', 'y')), false);
  assert.equal(r.hidden.has(id('y', 'x')), false);
  assert.equal(r.hidden.has(id('a', 'x')), false);
  assert.equal(r.implied, 1);
});

test('simplify reduce is not blocked by a violating back edge that closes a cycle', () => {
  const ns = ['a', 'b', 'c'].map((id) => n(id));
  const t = G.buildTree(ns);
  const es = [e('a', 'b'), e('b', 'c'), e('a', 'c'), e('c', 'a', true)]; // c->a violating closes a cycle
  const g = G.visibleGraph(t, es, new Set());
  const r = G.simplify(g, { reduce: true });
  assert.equal(r.hidden.has('e:a\u0000c'), true);
  assert.equal(r.hidden.has('e:c\u0000a'), false);
});

test('autoMinCount picks the smallest threshold that fits, ignoring violations', () => {
  const ns = ['a', 'b', 'c', 'd'].map((id) => n(id));
  const tr = G.buildTree(ns);
  // a->b x5, a->c x1, b->c x1, a->d x1 (violating)
  const es = [];
  for (let i = 0; i < 5; i++) es.push(e('a', 'b'));
  es.push(e('b', 'c'), e('c', 'd'), e('a', 'd', true));
  const g = G.visibleGraph(tr, es, new Set());
  assert.equal(G.autoMinCount(g, 10, false), 1);
  assert.equal(G.autoMinCount(g, 1, false), 2);  // only the x5 link survives
});
