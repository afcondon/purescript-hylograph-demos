// Does the demo do what it says, in a real browser?
//
//   make force-labels        (from the demos root: builds and bundles)
//   node force-labels/check.mjs [port]
//
// Serves public/ itself, drives headless Chrome, and reads the picture back
// out of the DOM: the rectangles and circles as drawn. At rest, with the
// labels free:
//   1. no two label rectangles overlap, and no label rectangle overlaps a
//      dot's disc. TOLERANCE: none. The rectangles and discs tested are the
//      ones drawn; in the simulation each collides 1.25 px larger all round,
//      and that margin is what the collision is allowed to give;
//   2. every label rectangle is inside the plot;
//   3. every label's text, as the browser set it, fits its rectangle (the
//      rectangle's width is computed from the text's length, not measured);
// for the year the page opens with, after each change of year, and on both
// kernels. With the labels fixed at their dots the same count is far from
// zero, which is the overlap the forces prevent. Two loads give the same
// arrangement to the last digit. Screenshots go to screenshots/.
import { createServer } from 'node:http';
import { readFileSync, mkdirSync } from 'node:fs';
import { dirname, extname, join, normalize } from 'node:path';
import { fileURLToPath, pathToFileURL } from 'node:url';

const here = dirname(fileURLToPath(import.meta.url));
const output = join(here, '../output');
const films = join(here, '../../../hylograph-films/a-join-is-a-fold/node_modules');
const { default: puppeteer } = await import(pathToFileURL(join(films, 'puppeteer-core/lib/esm/puppeteer/puppeteer-core.js')));
const CHROME = '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome';
const PORT = Number(process.argv[2] || 4177);
const { frame } = await import(pathToFileURL(join(output, 'Labels.Plot/index.js')));

const types = { '.html': 'text/html', '.js': 'text/javascript', '.css': 'text/css', '.json': 'application/json', '.wasm': 'application/wasm' };
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
  await page.setViewport({ width: 1360, height: 1160 });
  page.on('pageerror', (error) => problems.push(String(error)));
  page.on('console', (message) => { if (message.type() === 'error') problems.push(message.text()); });
  await page.goto(`http://127.0.0.1:${PORT}/`);
  await page.waitForSelector('#stage .labels rect', { timeout: 60000 });
  return page;
};
const press = (page, choice) => page.click(`.button[data-choice="${choice}"]`);
const moving = (page) => page.waitForFunction(() => document.querySelector('#readout .figures').dataset.settled === 'false', { timeout: 10000 });
const atRest = (page) => page.waitForFunction(() => document.querySelector('#readout .figures').dataset.settled === 'true', { timeout: 180000 });
const figure = (page, name) => page.$eval(`#readout .figure[data-figure="${name}"] .value`, (v) => v.textContent);
const ticks = (page) => page.$eval('#readout .figures', (r) => Number(r.dataset.ticks));
const shot = (page, name) => page.screenshot({ path: join(here, 'screenshots', `${name}.png`) });

// The picture, as drawn.
const picture = (page) => page.evaluate(() => {
  const number = (element, name) => Number(element.getAttribute(name));
  const texts = new Map([...document.querySelectorAll('#stage .labels text')].map((t) => [t.textContent, t]));
  return {
    labels: [...document.querySelectorAll('#stage .labels rect')].map((r) => {
      const text = texts.get(r.dataset.name);
      const box = text.getBBox();
      return { name: r.dataset.name, x: number(r, 'x'), y: number(r, 'y'), width: number(r, 'width'), height: number(r, 'height'), text: { x: box.x, width: box.width, family: getComputedStyle(text).fontFamily } };
    }),
    dots: [...document.querySelectorAll('#stage .dots circle')].map((c) => ({ name: c.dataset.name, x: number(c, 'cx'), y: number(c, 'cy'), r: number(c, 'r') })),
    leaders: [...document.querySelectorAll('#stage .leaders line')].map((l) => Math.hypot(number(l, 'x2') - number(l, 'x1'), number(l, 'y2') - number(l, 'y1'))),
  };
});

const judge = ({ labels, dots, leaders }) => {
  let labelPairs = 0, onDots = 0, outside = 0, textOut = 0, nearestPair = Infinity, nearestDot = Infinity;
  const byName = new Map(dots.map((d) => [d.name, d]));
  for (let i = 0; i < labels.length; i++) {
    const a = labels[i];
    if (a.x < frame.left || a.x + a.width > frame.right || a.y < frame.top || a.y + a.height > frame.bottom) outside++;
    if (a.text.x < a.x - 0.25 || a.text.x + a.text.width > a.x + a.width + 0.25) textOut++;
    for (let j = i + 1; j < labels.length; j++) {
      const b = labels[j];
      // The gap between two rectangles: positive apart, negative overlapping.
      const gap = Math.max(Math.max(a.x, b.x) - Math.min(a.x + a.width, b.x + b.width), Math.max(a.y, b.y) - Math.min(a.y + a.height, b.y + b.height));
      if (gap < 0) labelPairs++;
      nearestPair = Math.min(nearestPair, gap);
    }
    for (const d of dots) {
      const nx = Math.max(a.x, Math.min(a.x + a.width, d.x)), ny = Math.max(a.y, Math.min(a.y + a.height, d.y));
      const gap = Math.hypot(nx - d.x, ny - d.y) - d.r;
      if (gap < 0) onDots++;
      nearestDot = Math.min(nearestDot, gap);
    }
  }
  // A leader as drawn runs from the dot's middle; its length here is from the dot's edge.
  const lengths = labels.map((a) => { const d = byName.get(a.name); const nx = Math.max(a.x, Math.min(a.x + a.width, d.x)), ny = Math.max(a.y, Math.min(a.y + a.height, d.y)); return Math.max(0, Math.hypot(nx - d.x, ny - d.y) - d.r); });
  return {
    count: labels.length, labelPairs, onDots, outside, textOut, nearestPair, nearestDot, drawnLeaders: leaders.length,
    meanLeader: lengths.reduce((a, b) => a + b, 0) / lengths.length, maxLeader: Math.max(...lengths),
    family: labels[0].text.family,
  };
};
// Every nation with a reading for the year is shown, not the most populous hundred.
const nations = { 1900: 145, 1950: 178, 1980: 178, 2008: 174 };
const clean = (what, verdict, took) => {
  expect(verdict.count === nations[what.slice(0, 4)] && verdict.labelPairs === 0 && verdict.onDots === 0 && verdict.outside === 0 && verdict.textOut === 0,
    `${what}: ${verdict.count} labels of ${nations[what.slice(0, 4)]}; ${verdict.labelPairs} pairs overlap, ${verdict.onDots} on a dot, ${verdict.outside} outside the plot, ${verdict.textOut} texts wider than their boxes` +
    ` (nearest two labels ${verdict.nearestPair.toFixed(2)} px apart, nearest label and dot ${verdict.nearestDot.toFixed(2)} px); leader from the dot's edge mean ${verdict.meanLeader.toFixed(1)} px, longest ${verdict.maxLeader.toFixed(1)} px; ${took} ticks`);
};

try {
  const page = await open();
  await atRest(page);
  const first = await picture(page);
  const opening = judge(first);
  clean('2008, PureScript kernel', opening, await ticks(page));
  expect(/Menlo/.test(opening.family), `the labels are set in ${opening.family}, the face their widths are computed for`);
  await shot(page, 'free-2008');

  // The labels fixed at their dots: what the forces prevent.
  await press(page, 'Labels fixed at their points'); await moving(page); await atRest(page);
  const fixed = judge(await picture(page));
  expect(fixed.labelPairs + fixed.onDots > 50, `fixed at their dots: ${fixed.labelPairs} pairs of labels overlap and ${fixed.onDots} labels cover a dot`);
  await shot(page, 'fixed-2008');
  await press(page, 'Labels free'); await moving(page); await atRest(page);
  const back = await picture(page);
  clean('2008 again, freed from their dots', judge(back), await ticks(page));
  const strayed = Math.max(...back.labels.map((l, i) => Math.hypot(l.x - first.labels[i].x, l.y - first.labels[i].y)));
  // Not an expectation any more. A label squeezed between two bodies goes through one of them, and which
  // one turns on the last digit of where it starts: freed a second time the labels start a hair from where
  // they first did, and come to rest in another arrangement, as clean. (Until 2026-11-18 the charges did the
  // arranging and the same arrangement came back, within half a pixel.)
  console.log(`     freed again, the labels come to rest in another arrangement: one is ${strayed.toFixed(0)} px from where it was`);

  // Other years.
  for (const year of ['1950', '1900', '1980']) {
    await press(page, year); await moving(page);
    if (year === '1950') {
      await page.waitForFunction(() => Number(document.querySelector('#readout .figures').dataset.ticks) >= 100);
      await shot(page, 'rearranging-to-1950');
    }
    await atRest(page);
    clean(`${year}, PureScript kernel`, judge(await picture(page)), await ticks(page));
    if (year === '1900') await shot(page, 'free-1900');
  }

  // The other kernel, on the same years.
  await press(page, 'Rust kernel (WASM)'); await moving(page); await atRest(page);
  clean('1980, Rust kernel', judge(await picture(page)), await ticks(page));
  for (const year of ['2008', '1950', '1900']) {
    await press(page, year); await moving(page); await atRest(page);
    clean(`${year}, Rust kernel`, judge(await picture(page)), await ticks(page));
  }
  console.log(`     per tick, mean over each kernel's last run: PureScript ${await figure(page, 'PureScript')}, Rust ${await figure(page, 'Rust')}`);
  await page.close();

  // A fresh page.
  const fresh = await open();
  await atRest(fresh);
  const again = await picture(fresh);
  await fresh.close();
  expect(JSON.stringify(again) === JSON.stringify(first), 'a fresh page comes to rest in exactly the same arrangement');
  expect(problems.length === 0, `the page reported nothing${problems.length ? `: ${[...new Set(problems)].join(' | ')}` : ''}`);
} finally {
  await browser.close();
  await new Promise((resolve) => server.close(resolve));
}

console.log(failures.length ? `${failures.length} failed` : 'all checks passed');
process.exit(failures.length ? 1 : 0);
