// Does the demo do what it says, in a real browser?
//
//   make force-downhill        (from the demos root: builds and bundles)
//   node force-downhill/check.mjs [port]
//
// Serves public/ itself, drives headless Chrome through the page's own
// controls (the slider, the play button, a click on a module), and reads the
// modules' places back out of the DOM. Checks:
//   1. at rest, every import's target is at least 40 px below its source
//      (the imports are read here from data/imports.json);
//   2. going back to moment k and playing on gives, at every later moment
//      looked at, exactly the places the first run had then (the page keeps
//      the first run as a ghost, and the two are compared digit for digit),
//      for several k;
//   3. going back, pinning a module and playing on gives a different end, in
//      which the pinned module has not moved; and doing the same in a fresh
//      page gives the same different end;
//   4. two loads give the same first run.
// Writes screenshots to screenshots/.
import { createServer } from 'node:http';
import { readFileSync, mkdirSync } from 'node:fs';
import { dirname, extname, join, normalize } from 'node:path';
import { fileURLToPath, pathToFileURL } from 'node:url';

const here = dirname(fileURLToPath(import.meta.url));
const films = join(here, '../../../hylograph-films/a-join-is-a-fold/node_modules');
const { default: puppeteer } = await import(pathToFileURL(join(films, 'puppeteer-core/lib/esm/puppeteer/puppeteer-core.js')));
const CHROME = '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome';
const PORT = Number(process.argv[2] || 4178);
const DROP = 40;
const PINNED = 'Hylograph.HATS.InterpreterTick';
const graph = JSON.parse(readFileSync(join(here, 'public/data/imports.json'), 'utf8'));

const types = { '.html': 'text/html', '.js': 'text/javascript', '.css': 'text/css', '.json': 'application/json' };
const server = createServer((request, response) => {
  const path = normalize(decodeURIComponent(new URL(request.url, 'http://x').pathname));
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

const open = async () => {
  const page = await browser.newPage();
  await page.setViewport({ width: 1360, height: 1180 });
  page.on('pageerror', (error) => problems.push(String(error)));
  page.on('console', (message) => { if (message.type() === 'error') problems.push(message.text()); });
  await page.goto(`http://127.0.0.1:${PORT}/`);
  await page.waitForSelector('#stage .modules rect', { timeout: 60000 });
  return page;
};
const reading = (page) => page.$eval('#readout .figures', (r) => ({ tick: Number(r.dataset.tick), moments: Number(r.dataset.moments), shown: Number(r.dataset.shown), playing: r.dataset.playing === 'true', finished: r.dataset.finished === 'true', branched: r.dataset.branched === 'true', downhill: Number(r.dataset.downhill) }));
const ended = (page) => page.waitForFunction(() => { const r = document.querySelector('#readout .figures'); return r.dataset.playing === 'false' && r.dataset.finished === 'true'; }, { timeout: 120000 });
// Places as the page wrote them (data-x, data-y: the numbers in full), by module name.
const places = (page, group) => page.$$eval(`#stage .${group} rect`, (all) => all.map((r) => [r.dataset.name, r.dataset.x, r.dataset.y]).sort());
const scrub = async (page, moment) => {
  await page.$eval('#scrub', (slider, value) => { slider.value = String(value); slider.dispatchEvent(new Event('input', { bubbles: true })); }, moment);
  await page.waitForFunction((value) => Number(document.querySelector('#readout .figures').dataset.shown) === value, {}, moment);
};
const play = async (page) => { await page.click('#play'); await ended(page); };
const same = (a, b) => JSON.stringify(a) === JSON.stringify(b);
const furthest = (a, b) => Math.max(...a.map(([, x, y], i) => Math.hypot(Number(x) - Number(b[i][1]), Number(y) - Number(b[i][2]))));
const shot = (page, name) => page.screenshot({ path: join(here, 'screenshots', `${name}.png`) });

const firstRun = async (page) => {
  await ended(page);
  return { end: await places(page, 'modules'), reading: await reading(page) };
};
const branch = async (page, moment) => {
  await scrub(page, moment);
  const before = (await places(page, 'modules')).find(([name]) => name === PINNED);
  // (Dispatched on the element itself: this early in the run the boxes are still on top of each other,
  // and a click at the box's middle could land on another.)
  await page.$eval(`#stage .modules rect[data-name="${PINNED}"]`, (box) => box.dispatchEvent(new MouseEvent('click', { bubbles: true })));
  await page.waitForFunction(() => document.querySelector('#readout .figures').dataset.branched === 'true');
  await play(page);
  const marked = await page.$$eval('#stage .modules rect.pinned', (all) => all.map((r) => r.dataset.name));
  return { end: await places(page, 'modules'), ghost: await places(page, 'ghost'), pinnedAt: before, marked };
};

try {
  const page = await open();
  const first = await firstRun(page);
  const moments = first.reading.moments;

  // 1. Downhill.
  const at = new Map(first.end.map(([name, x, y]) => [name, { x: Number(x), y: Number(y) }]));
  const drops = graph.imports.filter((link) => at.has(link.from) && at.has(link.to)).map((link) => at.get(link.to).y - at.get(link.from).y);
  const short = drops.filter((drop) => drop < DROP).length;
  expect(short === 0 && drops.length === 87, `at rest (tick ${first.reading.tick}, ${moments} moments): ${drops.length - short} of ${drops.length} imports have their target at least ${DROP} px below their source; the smallest drop is ${Math.min(...drops).toFixed(1)} px`);
  expect(first.reading.downhill === drops.length - short, `the page counts the same: ${first.reading.downhill}`);
  expect(same(await places(page, 'ghost'), first.end), 'the first run is kept: its ghost is where the modules are');
  await shot(page, 'at-rest');

  // 2. Back, and the same again.
  const replays = [];
  for (const [k, laters] of [[2, [10, 30]], [Math.floor(moments / 3), [Math.floor(moments / 2)]], [moments - 8, [moments - 3]]]) {
    await scrub(page, k);
    const there = await reading(page);
    const back = same(await places(page, 'modules'), await places(page, 'ghost'));
    if (k === Math.floor(moments / 3)) await shot(page, 'mid-scrub');
    await play(page);
    const endSame = same(await places(page, 'modules'), first.end);
    const after = await reading(page);
    let laterSame = true;
    for (const m of laters) { await scrub(page, m); laterSame = laterSame && same(await places(page, 'modules'), await places(page, 'ghost')); }
    await scrub(page, after.moments - 1);
    replays.push(`moment ${k} (tick ${there.tick})`);
    expect(back && endSame && laterSame && after.moments === moments && !after.branched,
      `back to moment ${k} (tick ${there.tick}) and played on: the same ${after.moments} moments, the same places at moments ${laters.join(', ')} and at the end (tick ${after.tick}), digit for digit`);
  }

  // 3. A different future.
  const k = 4;
  const branched = await branch(page, k);
  const moved = furthest(branched.end, first.end);
  const pinnedEnd = branched.end.find(([name]) => name === PINNED);
  expect(moved > 20, `back to moment ${k}, ${PINNED} pinned where it was, and played on: a different end (a module is ${moved.toFixed(0)} px from where the first run left it)`);
  expect(same(branched.marked, [PINNED]), `the pinned module is marked: ${branched.marked.join(', ')}`);
  expect(same(pinnedEnd, branched.pinnedAt), `the pinned module is where it was pinned: ${JSON.stringify(branched.pinnedAt)} then, ${JSON.stringify(pinnedEnd)} at the end`);
  expect(same(branched.ghost, first.end), 'the ghost is still the first run');
  const after = await reading(page);
  console.log(`     the different future: ${after.moments} moments, ${after.downhill} of 87 imports pointing down at its end`);
  await shot(page, 'a-different-future');
  await page.close();

  // 4. A fresh page: the same first run, and the same different future.
  const fresh = await open();
  const again = await firstRun(fresh);
  expect(same(again.end, first.end) && again.reading.moments === moments, 'a fresh page records the same first run, digit for digit');
  const branchedAgain = await branch(fresh, k);
  expect(same(branchedAgain.end, branched.end), 'and the same pin at the same moment gives the same different future, digit for digit');
  await fresh.close();
  expect(problems.length === 0, `the page reported nothing${problems.length ? `: ${[...new Set(problems)].join(' | ')}` : ''}`);
} finally {
  await browser.close();
  await new Promise((resolve) => server.close(resolve));
}

console.log(failures.length ? `${failures.length} failed` : 'all checks passed');
process.exit(failures.length ? 1 : 0);
