// Does the type-class grid do what it says, in a real browser?
//
//   make sigil-hats        (from the demos root: builds and bundles)
//   node sigil-hats/check.mjs [port]
//
// Serves public/ itself, drives headless Chrome, and reads the picture back
// out of the DOM. The expected counts and cells come from the demo's own data
// (ClassGrid.Data in ../output, which the build writes):
//   1. one card per class, one line per superclass edge, one label per group;
//   2. the layout comes to rest (the page logs nothing when it does, so: no
//      card moves for a second), every position is finite and every card is
//      inside the drawing (the SVG's viewBox);
//   3. "nodes cluster into grid cells": every card is nearer its own group's
//      cell centre than any other group's;
//   4. a fresh page comes to rest in exactly the same layout (the cards
//      start at fixed offsets from their cell and the engine's tie-breaking
//      is seeded);
//   5. the page reports no errors.
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
const PORT = Number(process.argv[2] || 4182);
const { allClasses, allEdges, groupCenterX, groupCenterY } = await import(pathToFileURL(join(output, 'ClassGrid.Data/index.js')));
const groups = [...new Set(allClasses.map((c) => c.group))];

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

const picture = (page) => page.evaluate(() => {
  const svg = document.querySelector('#grid-svg');
  const box = svg.getAttribute('viewBox').split(/\s+/).map(Number);
  return {
    box: { left: box[0], top: box[1], right: box[0] + box[2], bottom: box[1] + box[3] },
    cards: [...svg.querySelectorAll('#grid-nodes > g')].map((g) => {
      const [, x, y] = /translate\(([^,]+),([^)]+)\)/.exec(g.getAttribute('transform'));
      const rect = g.querySelector(':scope > rect');
      return { key: g.dataset.hatsKey, x: Number(x), y: Number(y), width: Number(rect.getAttribute('width')), height: Number(rect.getAttribute('height')) };
    }),
    edges: svg.querySelectorAll('#grid-edges line').length,
    labels: svg.querySelectorAll('.zoom-group > text').length,
  };
});
const open = async () => {
  const page = await browser.newPage();
  await page.setViewport({ width: 1400, height: 900 });
  page.on('pageerror', (error) => problems.push(String(error)));
  page.on('console', (message) => { if (message.type() === 'error') problems.push(message.text()); });
  await page.goto(`http://127.0.0.1:${PORT}/`);
  await page.waitForSelector('#grid-nodes > g', { timeout: 60000 });
  return page;
};
const rest = async (page) => {
  const start = Date.now();
  let before = await picture(page);
  for (;;) {
    await sleep(1000);
    const after = await picture(page);
    if (JSON.stringify(after) === JSON.stringify(before)) return { picture: after, took: (Date.now() - start) / 1000 };
    if (Date.now() - start > 180000) throw new Error('still moving after three minutes');
    before = after;
  }
};

try {
  const page = await open();
  const { picture: still, took } = await rest(page);
  const { box, cards } = still;
  const finite = cards.filter((c) => Number.isFinite(c.x) && Number.isFinite(c.y)).length;
  const inside = cards.filter((c) => c.x - c.width / 2 >= box.left && c.x + c.width / 2 <= box.right && c.y - c.height / 2 >= box.top && c.y + c.height / 2 <= box.bottom).length;
  expect(finite === cards.length && inside === cards.length, `at rest after ${took.toFixed(0)} s: ${finite} of ${cards.length} positions finite, ${inside} cards inside the drawing`);
  expect(cards.length === allClasses.length, `${cards.length} cards drawn of ${allClasses.length} classes`);
  expect(still.edges === allEdges.length, `${still.edges} lines drawn of ${allEdges.length} superclass edges`);
  expect(still.labels === groups.length, `${still.labels} group labels drawn of ${groups.length} groups`);

  const cell = (group) => ({ x: groupCenterX(group), y: groupCenterY(group) });
  const home = cards.filter((card) => {
    const own = allClasses.find((c) => String(c.id) === card.key).group;
    const distance = (group) => Math.hypot(card.x - cell(group).x, card.y - cell(group).y);
    return groups.every((group) => group === own || distance(own) < distance(group));
  }).length;
  expect(home === cards.length, `${home} of ${cards.length} cards are nearer their own group's cell than any other`);
  await page.screenshot({ path: join(here, 'screenshots', 'at-rest.png') });
  await page.close();

  const again = await open();
  const fresh = await rest(again);
  await again.close();
  expect(JSON.stringify(fresh.picture) === JSON.stringify(still), 'a fresh page comes to rest in exactly the same layout');
  expect(problems.length === 0, `the page reported nothing${problems.length ? `: ${[...new Set(problems)].join(' | ')}` : ''}`);
} catch (error) {
  expect(false, String(error));
} finally {
  await browser.close();
  await new Promise((resolve) => server.close(resolve));
}

console.log(failures.length ? `${failures.length} failed` : 'all checks passed');
process.exit(failures.length ? 1 : 0);
