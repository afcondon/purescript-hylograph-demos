// Does the Force Playground do what it says, in a real browser?
//
//   make simulation        (from the demos root: builds and bundles)
//   node simulation/check.mjs [port]
//
// Serves public/ itself, drives headless Chrome through the page's own
// controls, and reads the picture back out of the DOM. The network is
// generated at random on every load (Effect.Random), so the expected counts
// are the ones the page logs for the network it generated ("V2: Generated:
// N nodes, M links"), and there is no same-twice check: two loads draw two
// different networks. Checks:
//   1. N circles and M lines are drawn;
//   2. the simulation comes to rest (the page logs "converged"), and at rest
//      nothing moves for a second, every position is finite and every node is
//      inside the drawing (the SVG's viewBox);
//   3. hiding a category takes exactly that category's nodes off the page,
//      and showing it again brings them all back;
//   4. the Quadrants preset puts every node in its colour's quadrant
//      (blue NW, amber NE, red SW, green SE), and it comes to rest there;
//   5. a node dragged and let go: the layout comes to rest again;
//   6. Regenerate draws the new network's counts;
//   7. the page reports no errors.
// Screenshots go to screenshots/.
import { createServer } from 'node:http';
import { readFileSync, mkdirSync } from 'node:fs';
import { dirname, extname, join, normalize } from 'node:path';
import { fileURLToPath, pathToFileURL } from 'node:url';

const here = dirname(fileURLToPath(import.meta.url));
const films = join(here, '../../../hylograph-films/a-join-is-a-fold/node_modules');
const { default: puppeteer } = await import(pathToFileURL(join(films, 'puppeteer-core/lib/esm/puppeteer/puppeteer-core.js')));
const CHROME = '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome';
const PORT = Number(process.argv[2] || 4179);

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

// Colours by category, as Simple.categoryColor gives them, and where Quadrants sends each.
const categories = [
  { name: 'RESEARCH', fill: '#1e5a8c', quadrant: 'NW', inside: (x, y) => x < 0 && y < 0 },
  { name: 'INDUSTRY', fill: '#d4a024', quadrant: 'NE', inside: (x, y) => x > 0 && y < 0 },
  { name: 'GOVERNMENT', fill: '#c41e3a', quadrant: 'SW', inside: (x, y) => x < 0 && y > 0 },
  { name: 'COMMUNITY', fill: '#2d8b57', quadrant: 'SE', inside: (x, y) => x > 0 && y > 0 },
];

const state = { generated: [], converged: 0 };
const open = async () => {
  const page = await browser.newPage();
  await page.setViewport({ width: 1400, height: 1000 });
  page.on('pageerror', (error) => problems.push(String(error)));
  page.on('console', (message) => {
    const text = message.text();
    if (message.type() === 'error') problems.push(text);
    const generated = /Generated: (\d+) nodes, (\d+) links/.exec(text);
    if (generated) state.generated.push({ nodes: Number(generated[1]), links: Number(generated[2]) });
    if (/Simulation converged/.test(text)) state.converged++;
  });
  await page.goto(`http://127.0.0.1:${PORT}/`);
  await page.waitForSelector('#network-nodes circle', { timeout: 60000 });
  return page;
};
const until = async (condition, what, timeout = 120000) => {
  const start = Date.now();
  while (!condition()) {
    if (Date.now() - start > timeout) throw new Error(`timed out waiting for ${what}`);
    await new Promise((resolve) => setTimeout(resolve, 100));
  }
};
const nextRest = async (before) => until(() => state.converged > before, 'the simulation to converge');
const press = (page, title) => page.click(`.semiotic-button[title="${title}"]`);
const shot = (page, name) => page.screenshot({ path: join(here, 'screenshots', `${name}.png`) });
const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));

const picture = (page) => page.evaluate(() => {
  const number = (element, name) => Number(element.getAttribute(name));
  const box = document.querySelector('#network-force-svg').getAttribute('viewBox').split(/\s+/).map(Number);
  return {
    box: { left: box[0], top: box[1], right: box[0] + box[2], bottom: box[1] + box[3] },
    nodes: [...document.querySelectorAll('#network-nodes circle')].map((c) => ({ key: c.dataset.hatsKey, x: number(c, 'cx'), y: number(c, 'cy'), r: number(c, 'r'), fill: c.getAttribute('fill') })),
    links: document.querySelectorAll('#network-links line').length,
  };
});
const atRest = async (page, what) => {
  const first = await picture(page);
  await sleep(1000);
  const second = await picture(page);
  const still = JSON.stringify(first) === JSON.stringify(second);
  const { box } = second;
  const finite = second.nodes.filter((n) => Number.isFinite(n.x) && Number.isFinite(n.y)).length;
  const inside = second.nodes.filter((n) => n.x - n.r >= box.left && n.x + n.r <= box.right && n.y - n.r >= box.top && n.y + n.r <= box.bottom).length;
  expect(still && finite === second.nodes.length && inside === second.nodes.length,
    `${what}: at rest (nothing moved in a second: ${still}), ${finite} of ${second.nodes.length} positions finite, ${inside} inside the drawing`);
  return second;
};

try {
  const page = await open();
  await until(() => state.generated.length === 1, 'the generated network');
  const network = state.generated[0];
  await nextRest(0);
  const rest = await atRest(page, 'opening layout');
  expect(rest.nodes.length === network.nodes && rest.links === network.links,
    `drawn: ${rest.nodes.length} nodes of ${network.nodes} generated, ${rest.links} links of ${network.links}`);
  await shot(page, 'at-rest');

  // Hide a category, then show it again.
  const research = categories[0];
  const researchCount = rest.nodes.filter((n) => n.fill === research.fill).length;
  let converged = state.converged;
  await press(page, research.name);
  await nextRest(converged);
  await sleep(1500); // the exit transition runs on ticks; give it time to finish if the ticks have
  const hidden = await atRest(page, `${research.name} hidden`);
  expect(hidden.nodes.length === network.nodes - researchCount && hidden.nodes.every((n) => n.fill !== research.fill),
    `${research.name} hidden: ${hidden.nodes.length} nodes drawn, expected ${network.nodes} - ${researchCount}`);
  await shot(page, 'research-hidden');
  converged = state.converged;
  await press(page, research.name);
  await nextRest(converged);
  await sleep(1500);
  const shown = await atRest(page, `${research.name} shown again`);
  expect(shown.nodes.length === network.nodes, `${research.name} shown again: ${shown.nodes.length} nodes of ${network.nodes}`);

  // Quadrants.
  converged = state.converged;
  await press(page, 'QUADRANTS');
  await nextRest(converged);
  const quadrants = await atRest(page, 'Quadrants');
  for (const category of categories) {
    const members = quadrants.nodes.filter((n) => n.fill === category.fill);
    const placed = members.filter((n) => category.inside(n.x, n.y)).length;
    expect(placed === members.length, `Quadrants: ${placed} of ${members.length} ${category.name} nodes in the ${category.quadrant} quadrant`);
  }
  await shot(page, 'quadrants');

  // Back to Centered, then drag a node and let it go.
  converged = state.converged;
  await press(page, 'CENTERED');
  await nextRest(converged);
  const centered = await atRest(page, 'Centered again');
  const target = centered.nodes[0];
  const handle = await page.$(`#network-nodes circle[data-hats-key="${target.key}"]`);
  const bounds = await handle.boundingBox();
  converged = state.converged;
  await page.mouse.move(bounds.x + bounds.width / 2, bounds.y + bounds.height / 2);
  await page.mouse.down();
  await page.mouse.move(bounds.x + 150, bounds.y + 100, { steps: 10 });
  await page.mouse.up();
  await nextRest(converged);
  const dragged = await atRest(page, `node ${target.key} dragged and let go`);
  const moved = Math.max(...dragged.nodes.map((n) => { const was = centered.nodes.find((m) => m.key === n.key); return Math.hypot(n.x - was.x, n.y - was.y); }));
  expect(moved > 1, `the drag moved the layout: a node is ${moved.toFixed(1)} px from where it was`);

  // Regenerate.
  converged = state.converged;
  await page.click('.semiotic-button[title="Generate new network"]');
  await until(() => state.generated.length === 2, 'the regenerated network');
  await nextRest(converged);
  const regenerated = await atRest(page, 'regenerated network');
  const second = state.generated[1];
  expect(regenerated.nodes.length === second.nodes && regenerated.links === second.links,
    `regenerated: ${regenerated.nodes.length} nodes of ${second.nodes} generated, ${regenerated.links} links of ${second.links}`);
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
