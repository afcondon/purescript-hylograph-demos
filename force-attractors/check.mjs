// Does the demo do what it says, in a real browser?
//
//   make force-attractors        (from the demos root: builds and bundles)
//   node force-attractors/check.mjs [port]
//
// Serves public/ itself, drives headless Chrome, and checks:
//   1. after each layout button, once the readout says "at rest", every
//      circle is within TOLERANCE of the place that layout gives its node.
//      The places are computed here, in Node, by the same compiled
//      `Attractors.Layout.arrange` the page uses, so the simulation and the
//      DOM are what is under test;
//   2. the same sequence of buttons in a fresh page gives the same final
//      positions, to the last digit;
//   3. a button pressed in mid-flight finds the nodes already moving: their
//      first step is not towards the new target.
// Writes a screenshot of each layout at rest, and two in flight, to screenshots/.
import { createServer } from 'node:http';
import { readFileSync, mkdirSync } from 'node:fs';
import { dirname, extname, join, normalize } from 'node:path';
import { fileURLToPath, pathToFileURL } from 'node:url';

const here = dirname(fileURLToPath(import.meta.url));
const output = join(here, '../output');
const films = join(here, '../../../hylograph-films/a-join-is-a-fold/node_modules');
const { default: puppeteer } = await import(pathToFileURL(join(films, 'puppeteer-core/lib/esm/puppeteer/puppeteer-core.js')));
const CHROME = '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome';
const PORT = Number(process.argv[2] || 4175);
const TOLERANCE = 0.25; // px, in the picture's own coordinates
const SEQUENCE = ['Radial cluster', 'Circle pack', 'Treemap', 'Sunburst', 'Tree'];
const IN_FLIGHT = new Set(['Radial cluster', 'Treemap']); // also photographed 9 ticks after the press

// ---------------------------------------------------------------------------
// Where each layout puts each node, from the compiled PureScript
// ---------------------------------------------------------------------------
const ps = (module) => import(pathToFileURL(join(output, module, 'index.js')));
const Flare = await ps('Attractors.Flare');
const Layout = await ps('Attractors.Layout');
const flare = Flare.decode(JSON.parse(readFileSync(join(here, 'public/data/flare-2.json'), 'utf8'))).value0;
const entries = (map) => { const out = []; const walk = (n) => { if ('value5' in n) { walk(n.value4); out.push([n.value2, n.value3]); walk(n.value5); } }; walk(map); return out; };
const targets = new Map(Layout.allLayouts.map((layout) => [Layout.label(layout), new Map(entries(Layout.arrange(layout)(flare).targets))]));

// ---------------------------------------------------------------------------
// A server for public/
// ---------------------------------------------------------------------------
const types = { '.html': 'text/html', '.js': 'text/javascript', '.css': 'text/css', '.json': 'application/json', '.wasm': 'application/wasm' };
const server = createServer((request, response) => {
  const path = normalize(decodeURIComponent(new URL(request.url, 'http://x').pathname));
  try {
    const body = readFileSync(join(here, 'public', path === '/' ? 'index.html' : path));
    response.writeHead(200, { 'content-type': types[extname(path)] || 'text/html' }).end(body);
  } catch { response.writeHead(404).end(); }
});
await new Promise((resolve, reject) => server.once('error', reject).listen(PORT, '127.0.0.1', resolve));

// ---------------------------------------------------------------------------
// The browser
// ---------------------------------------------------------------------------
mkdirSync(join(here, 'screenshots'), { recursive: true });
const slug = (name) => name.toLowerCase().replace(/ /g, '-');
const browser = await puppeteer.launch({ executablePath: CHROME, headless: true });
const failures = [];
const problems = [];

const open = async () => {
  const page = await browser.newPage();
  await page.setViewport({ width: 1360, height: 1120 });
  page.on('pageerror', (error) => problems.push(String(error)));
  page.on('console', (message) => { if (message.type() === 'error') problems.push(message.text()); });
  await page.goto(`http://127.0.0.1:${PORT}/`);
  await page.waitForSelector('#stage .nodes circle');
  return page;
};
const press = (page, name) => page.click(`.button[data-layout="${name}"]`);
const atRest = (page, name) => page.waitForFunction(
  (layout) => { const r = document.querySelector('#readout .figures'); return r && r.dataset.layout === layout && r.dataset.settled === 'true'; },
  { timeout: 20000 }, name);
const ticks = (page) => page.$eval('#readout .figures', (r) => Number(r.dataset.ticks));
const circles = (page) => page.$$eval('#stage .nodes circle', (all) => all.map((c) => [c.dataset.path, c.getAttribute('cx'), c.getAttribute('cy')]));

const run = async (photograph) => {
  const page = await open();
  const finals = {};
  for (const name of SEQUENCE) {
    await press(page, name);
    if (photograph && IN_FLIGHT.has(name)) {
      await page.waitForFunction((layout) => { const r = document.querySelector('#readout .figures'); return r.dataset.layout === layout && Number(r.dataset.ticks) >= 9; }, {}, name);
      await page.screenshot({ path: join(here, 'screenshots', `in-flight-to-${slug(name)}.png`) });
    }
    await atRest(page, name);
    const settledIn = await ticks(page);
    const found = await circles(page);
    let furthest = 0;
    for (const [path, cx, cy] of found) {
      const target = targets.get(name).get(path);
      furthest = Math.max(furthest, Math.hypot(Number(cx) - target.x, Number(cy) - target.y));
    }
    const marked = await page.$$eval('.button.current', (all) => all.map((b) => b.dataset.layout));
    finals[name] = found;
    if (photograph) {
      await page.screenshot({ path: join(here, 'screenshots', `at-rest-${slug(name)}.png`) });
      console.log(`${name.padEnd(15)} ${found.length} circles, at rest after ${settledIn} ticks, furthest from target ${furthest.toFixed(3)} px`);
      if (found.length !== targets.get(name).size) failures.push(`${name}: ${found.length} circles for ${targets.get(name).size} nodes`);
      if (!(furthest <= TOLERANCE)) failures.push(`${name}: a circle is ${furthest.toFixed(3)} px from its target (tolerance ${TOLERANCE})`);
      if (marked.join() !== name) failures.push(`${name}: the buttons mark [${marked.join()}] as current`);
    }
  }
  await page.close();
  return finals;
};

try {
  const first = await run(true);
  const second = await run(false);
  const same = JSON.stringify(first) === JSON.stringify(second);
  console.log(`same sequence, fresh page: ${same ? 'identical final positions' : 'DIFFERENT final positions'}`);
  if (!same) failures.push('two runs of the same sequence ended in different positions');

  // Momentum. Press a button and compare each node's first step with the
  // straight line to its new target (the cosine of the angle between them).
  // From rest every node sets off straight at its target. Pressed in
  // mid-flight, a node is still travelling the way it was going.
  const heading = (page, name) => page.evaluate(async (layout, places) => {
    const read = () => new Map([...document.querySelectorAll('#stage .nodes circle')].map((c) => [c.dataset.path, [Number(c.getAttribute('cx')), Number(c.getAttribute('cy'))]]));
    document.querySelector(`.button[data-layout="${layout}"]`).click();
    const before = read();
    await new Promise((resolve) => requestAnimationFrame(resolve)); // the page's own frame, one tick, runs first
    const after = read();
    let sum = 0, count = 0;
    for (const [path, [x, y]] of before) {
      const [x1, y1] = after.get(path), [tx, ty] = places[path];
      const step = Math.hypot(x1 - x, y1 - y), way = Math.hypot(tx - x, ty - y);
      if (step > 1e-6 && way > 1e-6) { sum += ((x1 - x) * (tx - x) + (y1 - y) * (ty - y)) / (step * way); count++; }
    }
    return sum / count;
  }, name, Object.fromEntries([...targets.get(name)].map(([path, point]) => [path, [point.x, point.y]])));
  const page = await open();
  const fromRest = await heading(page, 'Circle pack');
  await page.waitForFunction(() => Number(document.querySelector('#readout .figures').dataset.ticks) >= 15);
  const inFlight = await heading(page, 'Treemap');
  await atRest(page, 'Treemap');
  await page.close();
  console.log(`momentum: first step against the line to the target, mean cosine ${fromRest.toFixed(3)} from rest, ${inFlight.toFixed(3)} when pressed in mid-flight`);
  if (!(fromRest > 0.99)) failures.push(`from rest, nodes do not set off towards their targets (mean cosine ${fromRest.toFixed(3)})`);
  if (!(inFlight < 0.9)) failures.push(`pressed in mid-flight, nodes set off straight at their new targets (mean cosine ${inFlight.toFixed(3)}): no momentum carried`);
  if (problems.length) failures.push(`the page reported: ${[...new Set(problems)].join(' | ')}`);
} finally {
  await browser.close();
  await new Promise((resolve) => server.close(resolve));
}

for (const failure of failures) console.log(`FAIL ${failure}`);
console.log(failures.length ? `${failures.length} failed` : 'all checks passed');
process.exit(failures.length ? 1 : 0);
