// Does the Honeycomb do what it says, in a real browser?
//
//   make graph        (from the demos root: builds and bundles)
//   node graph/check.mjs [port]
//
// The Honeycomb's six simulations are hylograph-simulation's stepped handles
// (Hylograph.Simulation.Stepped, default engine), one per graph, stepped by
// one animation loop for the page. The puzzle is generated at random on
// every load, so there is no same-twice check.
//
// Serves public/ itself, drives headless Chrome, and reads the picture back
// out of the DOM. Checks:
//   1. six graphs, each drawing one circle and one label per node of
//      Honeycomb.Types.allNodes (read from ../output, which the build writes);
//   2. each graph comes to rest: nothing moves for a second, every position
//      is finite and every node is inside its graph's viewBox; and graphs
//      with different edges are laid out differently ("each running its own
//      force-directed simulation"), and each graph's layout reflects its
//      own edges (joined nodes sit closer, on average, than unjoined ones);
//   3. "Paths connected: k / 6" counts the graphs marked connected;
//   4. clicking an edge in one graph toggles that edge in all six (each
//      graph's edges, read from where the lines are drawn, gain or lose
//      exactly that edge), the layout is reheated (something moves), and the
//      move is counted;
//   5. Solve connects all six;
//   6. the page reports no errors.
// Screenshots go to screenshots/.
import { createServer } from 'node:http';
import { readFileSync, mkdirSync } from 'node:fs';
import { dirname, extname, join, normalize } from 'node:path';
import { fileURLToPath, pathToFileURL } from 'node:url';

const here = dirname(fileURLToPath(import.meta.url));
const output = join(here, '../output');
const films = join(here, '../../../hylograph-films/a-join-is-a-fold/node_modules');
const { default: puppeteer } = await import(pathToFileURL(join(films, 'puppeteer-core/lib/esm/puppeteer/puppeteer-core.js')));
const CHROME = '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome';
const PORT = Number(process.argv[2] || 4181);
const Types = await import(pathToFileURL(join(output, 'Honeycomb.Types/index.js')));
const labels = Types.allNodes.map((node) => Types.showNodeId.show(node)).sort();
const GRAPHS = 6;

const types = { '.html': 'text/html', '.js': 'text/javascript', '.css': 'text/css', '.json': 'application/json' };
const server = createServer((request, response) => {
  const path = normalize(decodeURIComponent(new URL(request.url, 'http://x').pathname));
  // The page names no icon, so the browser asks for one; that is not the demo's fault.
  if (path === '/favicon.ico') return response.writeHead(204).end();
  try {
    const body = readFileSync(join(here, 'public', path === '/' ? 'index.html' : path));
    response.writeHead(200, { 'content-type': types[extname(path)] || 'text/html' }).end(body);
  } catch { response.writeHead(404).end(); }
});
await new Promise((resolve, reject) => server.once('error', reject).listen(PORT, '127.0.0.1', resolve));

mkdirSync(join(here, 'screenshots'), { recursive: true });
const browser = await puppeteer.launch({ executablePath: CHROME, headless: true });
const failures = [];
const problems = [];
const expect = (ok, what) => { console.log(`${ok ? 'ok  ' : 'FAIL'} ${what}`); if (!ok) failures.push(what); };
const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));
const shot = (page, name) => page.screenshot({ path: join(here, 'screenshots', `${name}.png`), fullPage: true });

// Each graph as drawn: its nodes (by label) and its edges (by the labels at their ends).
const picture = (page) => page.evaluate(() => [...document.querySelectorAll('.hex-container')].map((hex) => {
  const svg = hex.querySelector('svg.graph-svg-inner');
  const box = svg.getAttribute('viewBox').split(/\s+/).map(Number);
  const nodes = [...svg.querySelectorAll('g.node')].map((g) => {
    const c = g.querySelector('circle');
    return { label: g.querySelector('text').textContent, cx: c.getAttribute('cx'), cy: c.getAttribute('cy'), r: Number(c.getAttribute('r')), start: g.classList.contains('start'), end: g.classList.contains('end') };
  });
  const at = new Map(nodes.map((n) => [`${n.cx},${n.cy}`, n.label]));
  const edges = [...svg.querySelectorAll('g.edge')].map((g) => {
    const line = g.querySelector('line');
    return [at.get(`${line.getAttribute('x1')},${line.getAttribute('y1')}`), at.get(`${line.getAttribute('x2')},${line.getAttribute('y2')}`)].sort().join('-');
  });
  return { box: { left: box[0], top: box[1], right: box[0] + box[2], bottom: box[1] + box[3] }, nodes, firstEdge: edges[0], edges: [...edges].sort(), connected: hex.querySelector('.hex-status').textContent === 'connected' };
}));
// "connected", worked out here from what is drawn and not from what the page
// says: is there a walk along the drawn edges from the start node to the end?
const walkable = (graph) => {
  const start = graph.nodes.find((n) => n.start), end = graph.nodes.find((n) => n.end);
  if (!start || !end) return null;
  const seen = new Set([start.label]), queue = [start.label];
  while (queue.length) {
    const at = queue.shift();
    for (const edge of graph.edges) {
      const [a, b] = [edge.slice(0, edge.indexOf('-')), edge.slice(edge.indexOf('-') + 1)];
      const next = a === at ? b : b === at ? a : null;
      if (next !== null && !seen.has(next)) { seen.add(next); queue.push(next); }
    }
  }
  return seen.has(end.label);
};
const judgedRight = (graphs, what) => {
  const marked = graphs.every((graph) => graph.nodes.filter((n) => n.start).length === 1 && graph.nodes.filter((n) => n.end).length === 1);
  const right = graphs.filter((graph) => walkable(graph) === graph.connected).length;
  expect(marked && right === GRAPHS, `${what}: ${right} of ${GRAPHS} graphs are called connected exactly when the drawn edges join start to end (${graphs.map((graph) => (walkable(graph) ? 'y' : 'n') + (graph.connected ? 'Y' : 'N')).join(' ')}: walk, page)`);
};
// The solved dialog: is it drawn, what does it say, and does it cover any graph?
// A graph is covered if, with the graph scrolled to the middle of the window,
// the element at the graph's centre is not inside that graph.
const banner = (page) => page.evaluate(() => {
  const dialog = document.querySelector('.solved-dialog');
  if (!dialog) return { shown: false, text: '', covered: 0 };
  let covered = 0;
  for (const hex of document.querySelectorAll('.hex-container')) {
    hex.scrollIntoView({ block: 'center' });
    const r = hex.getBoundingClientRect();
    if (!hex.contains(document.elementFromPoint(r.left + r.width / 2, r.top + r.height / 2))) covered++;
  }
  window.scrollTo(0, 0);
  return { shown: true, text: dialog.textContent, covered, role: dialog.getAttribute('role') };
});
const press = (page, label) => page.evaluate((text) => [...document.querySelectorAll('button')].find((b) => b.textContent === text).click(), label);
// Each graph's own edges shape its layout: joined pairs closer on average than unjoined ones.
const reflectsEdges = (graphs, what) => {
  const shaped = graphs.filter((graph) => {
    const at = new Map(graph.nodes.map((n) => [n.label, { x: Number(n.cx), y: Number(n.cy) }]));
    const joined = new Set(graph.edges);
    const pairs = graph.nodes.flatMap((a, i) => graph.nodes.slice(i + 1).map((b) => [a.label, b.label].sort()));
    const mean = (list) => list.reduce((sum, [a, b]) => sum + Math.hypot(at.get(a).x - at.get(b).x, at.get(a).y - at.get(b).y), 0) / list.length;
    const near = pairs.filter((pair) => joined.has(pair.join('-'))), far = pairs.filter((pair) => !joined.has(pair.join('-')));
    return near.length === 0 || far.length === 0 || mean(near) < mean(far);
  }).length;
  expect(shaped === graphs.length, `${what}: in ${shaped} of ${graphs.length} graphs joined nodes sit closer on average than unjoined ones`);
};
const status = (page) => page.$eval('.status-bar', (bar) => bar.textContent);
const settle = async (page, what) => {
  const start = Date.now();
  let before = await picture(page);
  for (;;) {
    await sleep(1000);
    const after = await picture(page);
    if (JSON.stringify(after) === JSON.stringify(before)) break;
    if (Date.now() - start > 120000) { expect(false, `${what}: still moving after two minutes`); return after; }
    before = after;
  }
  const nodes = before.flatMap((graph) => graph.nodes.map((n) => ({ ...n, box: graph.box })));
  const finite = nodes.filter((n) => Number.isFinite(Number(n.cx)) && Number.isFinite(Number(n.cy))).length;
  const inside = nodes.filter((n) => Number(n.cx) - n.r >= n.box.left && Number(n.cx) + n.r <= n.box.right && Number(n.cy) - n.r >= n.box.top && Number(n.cy) + n.r <= n.box.bottom).length;
  expect(finite === nodes.length && inside === nodes.length,
    `${what}: at rest after ${((Date.now() - start) / 1000).toFixed(0)} s; ${finite} of ${nodes.length} positions finite, ${inside} inside their graph`);
  return before;
};

try {
  const page = await browser.newPage();
  await page.setViewport({ width: 1400, height: 1000 });
  page.on('pageerror', (error) => problems.push(String(error)));
  page.on('console', (message) => { if (message.type() === 'error') problems.push(message.text()); });
  await page.goto(`http://127.0.0.1:${PORT}/`);
  await page.waitForSelector('svg.graph-svg-inner g.node', { timeout: 60000 });

  const opening = await settle(page, 'opening puzzle');
  expect(opening.length === GRAPHS, `${opening.length} graphs drawn of ${GRAPHS}`);
  const complete = opening.filter((graph) => JSON.stringify(graph.nodes.map((n) => n.label).sort()) === JSON.stringify(labels)).length;
  expect(complete === GRAPHS, `${complete} of ${GRAPHS} graphs draw exactly the ${labels.length} nodes ${labels.join(' ')}`);
  const placed = opening.reduce((sum, graph) => sum + graph.edges.filter((edge) => !edge.includes('undefined')).length, 0);
  const drawn = opening.reduce((sum, graph) => sum + graph.edges.length, 0);
  expect(placed === drawn, `${placed} of ${drawn} edges drawn end on a node`);
  // "Six graphs, each running its own force-directed simulation": graphs with different edges are laid out differently.
  const layout = (graph) => graph.nodes.map((n) => `${n.label}@${n.cx},${n.cy}`).sort().join(' ');
  const edgeSets = new Set(opening.map((graph) => graph.edges.join(' '))).size;
  const layouts = new Set(opening.map(layout)).size;
  expect(layouts === edgeSets, `${edgeSets} different edge sets among the six graphs are laid out in ${layouts} different ways`);
  reflectsEdges(opening, 'opening puzzle');
  judgedRight(opening, 'opening puzzle');
  { const b = await banner(page); expect(b.shown === opening.every((graph) => walkable(graph)), `opening puzzle: the solved dialog is ${b.shown ? 'drawn' : 'not drawn'}, and the six are ${opening.every((graph) => walkable(graph)) ? '' : 'not '}all joined`); }
  const connected = opening.filter((graph) => graph.connected).length;
  expect((await status(page)).includes(`${connected} / 6`), `the status bar counts ${connected} connected: "${(await status(page)).trim()}"`);
  await shot(page, 'opening');

  // Toggle one edge in the first graph that has one.
  const graphIndex = opening.findIndex((graph) => graph.edges.length > 0);
  const edge = opening[graphIndex].firstEdge;
  await page.evaluate((index) => document.querySelectorAll('.hex-container')[index].querySelector('g.edge').dispatchEvent(new MouseEvent('click', { bubbles: true })), graphIndex);
  const toggled = await settle(page, `edge ${edge} toggled`);
  const flipped = toggled.filter((graph, i) => {
    const had = opening[i].edges.includes(edge), has = graph.edges.includes(edge);
    const others = (edges) => edges.filter((e) => e !== edge);
    return had !== has && JSON.stringify(others(graph.edges)) === JSON.stringify(others(opening[i].edges));
  }).length;
  const moved = Math.max(...toggled.flatMap((graph, i) => graph.nodes.map((n, j) => Math.hypot(Number(n.cx) - Number(opening[i].nodes[j].cx), Number(n.cy) - Number(opening[i].nodes[j].cy)))));
  expect(moved > 0.5, `the toggle reheats the layout: a node moved ${moved.toFixed(2)} px`);
  reflectsEdges(toggled, `edge ${edge} toggled`);
  judgedRight(toggled, `edge ${edge} toggled`);
  { const b = await banner(page); expect(b.shown === toggled.every((graph) => walkable(graph)), `edge ${edge} toggled: the solved dialog is ${b.shown ? 'drawn' : 'not drawn'}, and the drawn edges ${toggled.every((graph) => walkable(graph)) ? 'do' : 'do not'} join start to end in all six`); }
  expect(flipped === GRAPHS, `clicking ${edge} in graph ${graphIndex + 1}: ${flipped} of ${GRAPHS} graphs gained or lost exactly that edge`);
  expect((await status(page)).includes('Moves: 1'), `the move is counted: "${(await status(page)).trim()}"`);
  await shot(page, 'toggled');

  // Solve.
  await page.evaluate(() => [...document.querySelectorAll('.controls button')].find((b) => b.textContent === 'Solve').click());
  const solved = await settle(page, 'solved');
  expect(solved.every((graph) => graph.connected) && (await status(page)).includes('6 / 6'), `Solve: ${solved.filter((graph) => graph.connected).length} of ${GRAPHS} graphs connected; "${(await status(page)).trim()}"`);
  reflectsEdges(solved, 'solved');
  judgedRight(solved, 'solved');
  { const b = await banner(page); expect(b.shown && b.covered === 0 && b.role === 'dialog' && /All Paths Connected/.test(b.text) && /The solution, shown/.test(b.text), `solved: the dialog is drawn, covers ${b.covered} of the six graphs, and says the solution was shown ("${b.text.replace(/\s+/g, ' ').trim()}")`); }
  // Keep Looking puts it away, and it stays away while the puzzle stays solved.
  await page.screenshot({ path: join(here, 'screenshots', 'solved-dialog.png'), fullPage: false });
  await press(page, 'Keep Looking'); await sleep(60);
  expect((await banner(page)).shown === false && (await picture(page)).every((graph) => graph.connected), 'Keep Looking: the dialog is gone and the solved puzzle is still there');
  // The solved puzzle can still be played: one more toggle, and the page must judge it again.
  { const index = solved.findIndex((graph) => graph.edges.length > 0);
    await page.evaluate((i) => document.querySelectorAll('.hex-container')[i].querySelector('g.edge').dispatchEvent(new MouseEvent('click', { bubbles: true })), index);
    const after = await settle(page, 'solved, then one more toggle');
    judgedRight(after, 'solved, then one more toggle');
    const b = await banner(page);
    expect(b.shown === false, `solved, then one more toggle: the dialog is ${b.shown ? 'drawn' : 'not drawn'} (dismissed, or no longer solved: all joined ${after.every((graph) => walkable(graph))})`); }
  await shot(page, 'solved');
  // Many more positions: sixty toggles of whichever edge comes up, judged after each.
  // The dialog must be drawn when the six become joined, and not drawn whenever they are not.
  { let wrong = 0, bannerWrong = 0, calledSolved = 0, tried = 0, pick = 7, was = (await picture(page)).every((g) => walkable(g));
    for (let turn = 0; turn < 60; turn++) {
      const now = await picture(page);
      const withEdges = now.map((graph, i) => ({ i, n: graph.edges.length })).filter((g) => g.n > 0);
      if (withEdges.length === 0) break;
      pick = (pick * 1103515245 + 12345) % 2147483648;
      const graph = withEdges[pick % withEdges.length], edgeIndex = Math.floor(pick / 64) % graph.n;
      await page.evaluate(([i, e]) => document.querySelectorAll('.hex-container')[i].querySelectorAll('g.edge')[e].dispatchEvent(new MouseEvent('click', { bubbles: true })), [graph.i, edgeIndex]);
      await sleep(60);
      const after = await picture(page), all = after.every((g) => walkable(g));
      tried++;
      if (after.some((g) => walkable(g) !== g.connected)) wrong++;
      const drawn = (await banner(page)).shown;
      if ((!all && drawn) || (all && !was && !drawn)) bannerWrong++;
      if (all && drawn) await press(page, 'Keep Looking');
      was = all;
      if (all) calledSolved++;
    }
    expect(tried === 60 && wrong === 0 && bannerWrong === 0, `sixty more toggles: the page's "connected" was wrong in ${wrong} positions and the dialog in ${bannerWrong} (all six joined in ${calledSolved} of ${tried})`); }
  // Play again: a new puzzle, and the dialog only if that one is solved.
  { await press(page, 'New Puzzle'); await sleep(200);
    const fresh = await picture(page), b = await banner(page);
    judgedRight(fresh, 'new puzzle');
    expect(b.shown === fresh.every((graph) => walkable(graph)) && /Moves: 0/.test(await status(page)), `new puzzle: the dialog is ${b.shown ? 'drawn' : 'not drawn'}, the six are ${fresh.every((graph) => walkable(graph)) ? '' : 'not '}all joined, and the moves are back to 0`); }
  await page.close();
  expect(problems.length === 0, `the page reported nothing${problems.length ? `: ${[...new Set(problems)].join(' | ')}` : ''}`);
} catch (error) {
  expect(false, String(error));
} finally {
  await browser.close();
  await new Promise((resolve) => server.close(resolve));
}

console.log(failures.length ? `${failures.length} failed` : 'all checks passed');
process.exit(failures.length ? 1 : 0);
