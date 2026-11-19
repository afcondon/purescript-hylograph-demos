// Does the Les Misérables page do what it says, in a real browser?
//
//   make onion        (from the demos root: builds and bundles)
//   node onion/check.mjs [port]
//
// Only lesmis.html runs a force simulation; the catalogue and recipes pages
// are not checked here. Serves public/ itself, drives headless Chrome, and
// reads the picture back out of the DOM. The expected counts come from the
// demo's own data (Demo.LesMis in ../output, which the build writes):
//   1. one circle per character, one pen line per co-occurrence, a label on
//      every node whose radius (from its degree) is over 9, and a watercolour
//      blob for every group;
//   2. the simulation comes to rest (the page logs "converged"), and at rest
//      nothing moves for a second, every position is finite and every node is
//      inside the drawing (the SVG's viewBox);
//   3. a fresh page comes to rest in exactly the same layout (the nodes start
//      on a grid and the engine's tie-breaking is seeded);
//   4. the page reports no errors.
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
const PORT = Number(process.argv[2] || 4180);
const { lesMisNodes, lesMisLinks, nodeScale, svgW, svgH } = await import(pathToFileURL(join(output, 'Demo.LesMis/index.js')));

// What the data says should be drawn.
const degree = (id) => lesMisLinks.filter((link) => link.source === id || link.target === id).length;
const radius = (id) => (4 + Math.sqrt(degree(id)) * 2) * nodeScale;
const expected = {
  nodes: lesMisNodes.length,
  edges: lesMisLinks.length,
  labels: lesMisNodes.filter((node) => radius(node.id) > 9).length,
  groups: new Set(lesMisNodes.map((node) => node.group)).size,
};

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

const open = async () => {
  const page = await browser.newPage();
  await page.setViewport({ width: 1400, height: 1000 });
  const run = { page, converged: false, started: Date.now() };
  page.on('pageerror', (error) => problems.push(String(error)));
  page.on('console', (message) => {
    if (message.type() === 'error') problems.push(message.text());
    if (/Simulation converged/.test(message.text())) run.converged = true;
  });
  await page.goto(`http://127.0.0.1:${PORT}/lesmis.html`);
  await page.waitForSelector('#lesmis-wrapper circle', { timeout: 60000 });
  return run;
};
const rest = async (run) => {
  while (!run.converged) {
    if (Date.now() - run.started > 180000) throw new Error('timed out waiting for the simulation to converge');
    await sleep(200);
  }
  return (Date.now() - run.started) / 1000;
};
const picture = (page) => page.evaluate(() => {
  const number = (element, name) => Number(element.getAttribute(name));
  const svg = document.querySelector('#lesmis-wrapper svg');
  const box = svg.getAttribute('viewBox').split(/\s+/).map(Number);
  return {
    box: { left: box[0], top: box[1], right: box[0] + box[2], bottom: box[1] + box[3] },
    nodes: [...svg.querySelectorAll('circle')].map((c) => ({ x: number(c, 'cx'), y: number(c, 'cy'), r: number(c, 'r') })),
    edges: [...svg.querySelectorAll('path')].filter((p) => p.getAttribute('fill') === 'none' && p.getAttribute('stroke') === '#1a1612').length,
    labels: svg.querySelectorAll('text').length,
    blobs: [...svg.querySelectorAll(':scope > g[transform]')].length,
  };
});
const shot = (page, name) => page.screenshot({ path: join(here, 'screenshots', `${name}.png`) });

try {
  const run = await open();
  const took = await rest(run);
  const first = await picture(run.page);
  await sleep(1000);
  const second = await picture(run.page);
  const { box } = second;
  const finite = second.nodes.filter((n) => Number.isFinite(n.x) && Number.isFinite(n.y)).length;
  const inside = second.nodes.filter((n) => n.x - n.r >= box.left && n.x + n.r <= box.right && n.y - n.r >= box.top && n.y + n.r <= box.bottom).length;
  expect(JSON.stringify(first) === JSON.stringify(second) && finite === second.nodes.length && inside === second.nodes.length,
    `at rest after ${took.toFixed(1)} s: nothing moved in a second, ${finite} of ${second.nodes.length} positions finite, ${inside} inside the drawing (${box.right - box.left} by ${box.bottom - box.top}; data ${svgW} by ${svgH})`);
  expect(second.nodes.length === expected.nodes, `${second.nodes.length} characters drawn of ${expected.nodes}`);
  expect(second.edges === expected.edges, `${second.edges} pen lines drawn of ${expected.edges} co-occurrences`);
  expect(second.labels === expected.labels, `${second.labels} labels drawn; ${expected.labels} characters have a radius over 9`);
  expect(second.blobs === expected.groups, `${second.blobs} watercolour blobs drawn of ${expected.groups} groups`);
  await shot(run.page, 'lesmis-at-rest');
  await run.page.close();

  const again = await open();
  await rest(again);
  const fresh = await picture(again.page);
  await again.page.close();
  expect(JSON.stringify(fresh.nodes) === JSON.stringify(second.nodes), 'a fresh page comes to rest in exactly the same layout');
  expect(problems.length === 0, `the page reported nothing${problems.length ? `: ${[...new Set(problems)].join(' | ')}` : ''}`);
} catch (error) {
  expect(false, String(error));
} finally {
  await browser.close();
  await new Promise((resolve) => server.close(resolve));
}

console.log(failures.length ? `${failures.length} failed` : 'all checks passed');
process.exit(failures.length ? 1 : 0);
