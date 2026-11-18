// Does the demo do what it says, in a real browser?
//
//   make force-territories        (from the demos root: builds and bundles)
//   node force-territories/check.mjs [port]
//
// Serves public/ itself, drives headless Chrome, and checks:
//   1. what is drawn: 21 territories, 8 crossings, 18 bridges, 77 characters,
//      254 meetings;
//   2. in territories, at rest, every character who is not a crossing is
//      inside its territory, and every crossing is within its plaza's radius
//      (plus the play of its post) of the rim of every territory it belongs
//      to. The geography is computed here, in Node, by the same compiled
//      `Territories.Geography.geography` the page uses, so the simulation and
//      the DOM are what is under test;
//   3. there and back: after open country and territories again, twice, the
//      characters are where they were, in both modes;
//   4. a fresh page gives the same two resting places, to the last digit.
// Writes screenshots of both resting places and of a walk to screenshots/.
import { createServer } from 'node:http';
import { readFileSync, mkdirSync } from 'node:fs';
import { dirname, extname, join, normalize } from 'node:path';
import { fileURLToPath, pathToFileURL } from 'node:url';

const here = dirname(fileURLToPath(import.meta.url));
const output = join(here, '../output');
const films = join(here, '../../../hylograph-films/a-join-is-a-fold/node_modules');
const { default: puppeteer } = await import(pathToFileURL(join(films, 'puppeteer-core/lib/esm/puppeteer/puppeteer-core.js')));
const CHROME = '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome';
const PORT = Number(process.argv[2] || 4176);
const INSIDE = 0.01;   // px a character may be outside its territory
const POST = 3.05;     // px a crossing may be from the point the layout gave it (Physics.postRadius, and rounding)
const RETURNS = 0.05;  // px between a resting place and the same one a round trip later

// ---------------------------------------------------------------------------
// The geography, from the compiled PureScript
// ---------------------------------------------------------------------------
const ps = (module) => import(pathToFileURL(join(output, module, 'index.js')));
const Story = await ps('Territories.Story');
const Geography = await ps('Territories.Geography');
const story = Story.decode(JSON.parse(readFileSync(join(here, 'public/data/miserables.json'), 'utf8'))).value0;
const geography = Geography.geography(story);
const crossings = new Map(geography.crossings.map((crossing) => [crossing.character, crossing]));
const homes = new Map(story.characters.filter((c) => !crossings.has(c)).map((c) => [c, geography.territories.find((t) => t.members.includes(c))]));

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
const browser = await puppeteer.launch({ executablePath: CHROME, headless: true });
const failures = [];
const problems = [];
const expect = (ok, what) => { console.log(`${ok ? 'ok  ' : 'FAIL'} ${what}`); if (!ok) failures.push(what); };

const open = async () => {
  const page = await browser.newPage();
  await page.setViewport({ width: 1360, height: 1140 });
  page.on('pageerror', (error) => problems.push(String(error)));
  page.on('console', (message) => { if (message.type() === 'error') problems.push(message.text()); });
  await page.goto(`http://127.0.0.1:${PORT}/`);
  await page.waitForSelector('#stage .characters circle', { timeout: 60000 });
  return page;
};
const press = (page, mode) => page.click(`.button[data-mode="${mode}"]`);
const atRest = (page, mode) => page.waitForFunction(
  (wanted) => { const r = document.querySelector('#readout .figures'); return r && r.dataset.mode === wanted && r.dataset.settled === 'true'; },
  { timeout: 120000 }, mode);
const ticks = (page) => page.$eval('#readout .figures', (r) => Number(r.dataset.ticks));
const places = (page) => page.$$eval('#stage .characters circle', (all) => all.map((c) => [c.dataset.name, c.getAttribute('cx'), c.getAttribute('cy'), c.getAttribute('r')]));
const furthestApart = (a, b) => Math.max(...a.map(([, x, y], i) => Math.hypot(Number(x) - Number(b[i][1]), Number(y) - Number(b[i][2]))));
const shot = async (page, name) => { await new Promise((resolve) => setTimeout(resolve, 700)); await page.screenshot({ path: join(here, 'screenshots', `${name}.png`) }); };

try {
  const page = await open();

  // 1. What is drawn.
  const drawn = await page.evaluate(() => ({
    territories: document.querySelectorAll('#stage circle.territory').length,
    crossings: document.querySelectorAll('#stage circle.crossing').length,
    bridges: document.querySelectorAll('#stage line.bridge').length,
    characters: document.querySelectorAll('#stage .characters circle').length,
    meetings: document.querySelectorAll('#stage .meetings line').length,
    named: [...document.querySelectorAll('#stage .names text')].map((t) => t.textContent).sort(),
    current: [...document.querySelectorAll('.button.current')].map((b) => b.dataset.mode),
  }));
  expect(drawn.territories === 21 && drawn.crossings === 8 && drawn.bridges === 18, `drawn: ${drawn.territories} territories, ${drawn.crossings} crossings, ${drawn.bridges} bridges (21, 8, 18)`);
  expect(drawn.characters === 77 && drawn.meetings === 254, `drawn: ${drawn.characters} characters, ${drawn.meetings} meetings (77, 254)`);
  expect(JSON.stringify(drawn.named) === JSON.stringify([...crossings.keys()].sort()), `the crossings are named: ${drawn.named.join(', ')}`);
  expect(drawn.current.join() === 'Territories', 'the page opens in territories');

  // 2. In territories, at rest.
  await atRest(page, 'Territories');
  const territories = [await places(page)];
  const inside = (found) => {
    let outside = -Infinity, fromRim = -Infinity, fromPost = -Infinity, plaza = 0;
    for (const [name, cx, cy, r] of found) {
      const x = Number(cx), y = Number(cy);
      const crossing = crossings.get(name);
      if (crossing) {
        fromPost = Math.max(fromPost, Math.hypot(x - crossing.x, y - crossing.y));
        for (const index of crossing.territories) {
          const land = geography.territories[index];
          const gap = Math.abs(Math.hypot(x - land.x, y - land.y) - land.radius);
          if (gap - crossing.plaza > fromRim) { fromRim = gap - crossing.plaza; }
          plaza = Math.max(plaza, gap);
        }
      } else {
        const land = homes.get(name);
        outside = Math.max(outside, Math.hypot(x - land.x, y - land.y) + Number(r) - land.radius);
      }
    }
    return { outside, fromRim, fromPost, plaza };
  };
  const first = inside(territories[0]);
  expect(first.outside <= INSIDE, `every character who is not a crossing is inside its territory (the nearest to leaving is ${(-first.outside).toFixed(2)} px inside)`);
  expect(first.fromRim <= POST && first.fromPost <= POST, `every crossing is within its plaza of each of its territories: at most ${first.fromRim.toFixed(2)} px beyond its plaza's radius, ${first.fromPost.toFixed(2)} px from its post; the furthest a crossing is from a territory's rim is ${first.plaza.toFixed(1)} px`);
  await shot(page, 'at-rest-territories');

  // 3. There and back, twice.
  const openCountry = [];
  const walks = [];
  for (const trip of [1, 2]) {
    await press(page, 'Open country');
    if (trip === 1) {
      await page.waitForFunction(() => { const r = document.querySelector('#readout .figures'); return r.dataset.mode === 'Open country' && Number(r.dataset.ticks) >= 7; });
      await page.screenshot({ path: join(here, 'screenshots', 'walking-to-open-country.png') });
    }
    await atRest(page, 'Open country');
    walks.push(`open country in ${await ticks(page)} ticks`);
    openCountry.push(await places(page));
    if (trip === 1) await shot(page, 'at-rest-open-country');
    await press(page, 'Territories');
    await atRest(page, 'Territories');
    walks.push(`territories in ${await ticks(page)} ticks`);
    territories.push(await places(page));
  }
  console.log(`     walks: ${walks.join(', ')}`);
  const returned = { territories: Math.max(furthestApart(territories[0], territories[1]), furthestApart(territories[1], territories[2])), openCountry: furthestApart(openCountry[0], openCountry[1]) };
  expect(returned.territories <= RETURNS, `back in territories the characters are where they were: within ${returned.territories.toFixed(4)} px, over two round trips`);
  expect(returned.openCountry <= RETURNS, `back in open country the characters are where they were: within ${returned.openCountry.toFixed(4)} px`);
  const again = inside(territories[2]);
  expect(again.outside <= INSIDE && again.fromRim <= POST, 'and the territories hold after the round trips');
  const moved = furthestApart(territories[0], openCountry[0]);
  expect(moved > 100, `the two resting places are different places: a character moves up to ${moved.toFixed(0)} px between them`);
  await page.close();

  // 4. A fresh page.
  const fresh = await open();
  await atRest(fresh, 'Territories');
  const freshTerritories = await places(fresh);
  await press(fresh, 'Open country');
  await atRest(fresh, 'Open country');
  const freshOpenCountry = await places(fresh);
  await fresh.close();
  expect(JSON.stringify(freshTerritories) === JSON.stringify(territories[0]) && JSON.stringify(freshOpenCountry) === JSON.stringify(openCountry[0]), 'a fresh page comes to rest in exactly the same places, in both modes');
  expect(problems.length === 0, `the page reported nothing${problems.length ? `: ${[...new Set(problems)].join(' | ')}` : ''}`);
} finally {
  await browser.close();
  await new Promise((resolve) => server.close(resolve));
}

console.log(failures.length ? `${failures.length} failed` : 'all checks passed');
process.exit(failures.length ? 1 : 0);
