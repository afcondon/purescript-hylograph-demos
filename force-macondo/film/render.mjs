// Render the Macondo page as a film, frame by frame, with its soundtrack.
//
//   node film/render.mjs                  the whole film  -> film/out/cien-anos-de-soledad.mp4
//   node film/render.mjs --silence        the same, ending in silence instead of the wind
//   node film/render.mjs --mix            the sound only, laid again on film/out/silent.mp4
//   node film/render.mjs still 175 2575   single frames   -> film/out/still-00175.png ...
//
// The page is opened with ?film: nothing runs by itself, and the page is
// stepped a frame at a time through window.macondo.step(n) (60 frames to the
// second), at the film's own pace (Macondo.Script.film). The sound is laid
// against the page's own cues (window.macondo.cues):
//
//   - "Batalla de Peralonso" (Carlos Romero, Victor, Bogotá, 15 November
//     1913), to 1:30, from the first frame: the command "¡armas al hombro!"
//     (0:58.85) falls a beat after "fusilamiento," is fully up, and his
//     "¡Colombia!" (1:27.15) on the frame the generations begin to be
//     painted. The render refuses if those drift more than a tenth of a
//     second apart.
//   - "¡Que mujeres!" (Orquesta de la Unión Musical de Bogotá, cond. Jerónimo
//     Velasco, Victor, Bogotá, 10 November 1913), from its first note,
//     crossfaded in as Peralonso ends, and gone before the last sentence
//     begins: it is too light for the novel's last lines.
//   - The wind, under the last sentence ("…sería arrasada por el viento"),
//     made here from noise: brown noise for the roar, a resonant band of pink
//     noise for the howl, a slow phaser for the swirl, gusts from slow sines
//     beating against each other. Or silence: node film/render.mjs --silence
//
// Both are Library of Congress National Jukebox transfers on the Internet
// Archive, public domain (first published 1913; Music Modernization Act).
// Uses the films repo's puppeteer-core and the system's ffmpeg.
import { createServer } from 'node:http';
import { readFileSync, mkdirSync, existsSync } from 'node:fs';
import { spawn } from 'node:child_process';
import { dirname, extname, join, normalize } from 'node:path';
import { fileURLToPath, pathToFileURL } from 'node:url';

const here = dirname(fileURLToPath(import.meta.url));
const pub = join(here, '..', 'public');
const out = join(here, 'out');
const audio = join(here, 'audio');
const films = join(here, '../../../../hylograph-films/a-join-is-a-fold/node_modules');
const { default: puppeteer } = await import(pathToFileURL(join(films, 'puppeteer-core/lib/esm/puppeteer/puppeteer-core.js')));
const CHROME = process.env.CHROME || '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome';
const FPS = 60;
const PORT = 4189;

// The recordings, and where in them things happen (seconds).
const PERALONSO = { file: join(audio, 'peralonso.mp3'), command: 58.85, to: 90.0, shout: 87.15 };
const MUJERES = { file: join(audio, 'que-mujeres.mp3'), from: 1.3 };
const LEVEL = '-3dB'; // both measure about -16.7 LUFS; a little under that
const CROSSFADE = 1.5;

mkdirSync(out, { recursive: true });
const types = { '.html': 'text/html', '.js': 'text/javascript', '.css': 'text/css', '.json': 'application/json', '.svg': 'image/svg+xml', '.woff2': 'font/woff2' };
const server = createServer((q, r) => {
  try {
    const p = normalize(join(pub, decodeURIComponent(q.url.split('?')[0] === '/' ? '/index.html' : q.url.split('?')[0])));
    if (!p.startsWith(pub)) throw new Error('outside');
    r.writeHead(200, { 'content-type': types[extname(p)] || 'application/octet-stream' });
    r.end(readFileSync(p));
  } catch { r.writeHead(404); r.end(); }
}).listen(PORT, '127.0.0.1');

const browser = await puppeteer.launch({ executablePath: CHROME, headless: true, args: ['--hide-scrollbars', '--force-color-profile=srgb'] });
const page = await browser.newPage();
await page.setViewport({ width: 1920, height: 1080, deviceScaleFactor: 1 });
let failed = null;
page.on('pageerror', (e) => { console.error('page error:', e.message); failed = e; });
page.on('console', (m) => { if (m.type() === 'error') { console.error('console error:', m.text()); failed = new Error(m.text()); } });
await page.goto(`http://127.0.0.1:${PORT}/?film`, { waitUntil: 'networkidle0' });
await page.waitForFunction(() => window.macondo && window.macondo.ready, { timeout: 60000 });

// Every face the page uses, loaded before the first frame: never render in a fallback.
const wanted = ['20px Fell', 'italic 20px Fell', '20px "Fell SC"'];
await page.evaluate((fs) => Promise.all(fs.map((f) => document.fonts.load(f))), wanted);
const faces = await page.evaluate((fs) => fs.map((f) => document.fonts.check(f)), wanted);
if (faces.includes(false)) { console.error('fonts did not load; refusing to render in a fallback face'); process.exit(1); }

const cues = await page.evaluate(() => window.macondo.cues);
const seconds = (frame) => frame / FPS;

// The sound, from the cues. Peralonso plays from the first frame, from
// wherever in the recording puts the command a beat after "fusilamiento," is
// fully up.
const fade = 36; // Macondo.Script.film.fadeIn
const commandAt = seconds(cues.fusilamiento + fade) + 0.48;
const from = PERALONSO.command - commandAt; // where in the recording the film begins
const shoutAt = PERALONSO.shout - from;
const drift = shoutAt - seconds(cues.generations);
console.log(`Peralonso from ${from.toFixed(2)} s; ¡armas al hombro! at ${commandAt.toFixed(2)} s; ¡Colombia! ${shoutAt.toFixed(2)} s; generations ${seconds(cues.generations).toFixed(2)} s (${drift >= 0 ? '+' : ''}${drift.toFixed(2)} s)`);
if (Math.abs(drift) > 0.1) { console.error('the shout and the generations have drifted apart: retime Macondo.Script.film'); process.exit(1); }
const peralonsoEnd = PERALONSO.to - from;
const mujeresAt = peralonsoEnd - CROSSFADE;
const length = seconds(cues.end);
const silence = process.argv.includes('--silence');

const shoot = async () => {
  if (failed) { await browser.close(); server.close(); process.exit(1); }
  return page.screenshot({ type: 'png', optimizeForSpeed: true });
};

const args = process.argv.slice(2).filter((a) => !a.startsWith('--'));
if (args[0] === 'still') {
  let at = 0;
  for (const a of args.slice(1).map(Number).sort((x, y) => x - y)) {
    await page.evaluate((n) => window.macondo.step(n), a - at); at = a;
    const path = join(out, `still-${String(a).padStart(5, '0')}.png`);
    await page.screenshot({ path });
    console.log(path);
  }
} else {
  const silent = join(out, 'silent.mp4');
  const film = join(out, silence ? 'cien-anos-de-soledad-silencio.mp4' : 'cien-anos-de-soledad.mp4');
  // The picture, unless --mix asks only for the sound to be laid again on the
  // picture already rendered (film/out/silent.mp4, kept for this).
  if (!(process.argv.includes('--mix') && existsSync(silent))) {
    const ffmpeg = spawn('ffmpeg', ['-y', '-loglevel', 'error', '-f', 'image2pipe', '-framerate', String(FPS), '-i', '-',
      '-c:v', 'libx264', '-preset', 'medium', '-crf', '16', '-pix_fmt', 'yuv420p', silent], { stdio: ['pipe', 'inherit', 'inherit'] });
    for (let i = 0; i < cues.end; i++) {
      const png = await shoot();
      if (!ffmpeg.stdin.write(png)) await new Promise((r) => ffmpeg.stdin.once('drain', r));
      await page.evaluate(() => window.macondo.step(1));
      if (i % 600 === 0) console.log(`frame ${i}/${cues.end}`);
    }
    ffmpeg.stdin.end();
    await new Promise((r) => ffmpeg.on('close', r));
  }

  // Peralonso: from where the film begins, faded in softly (it starts mid
  // recording) and out under the crossfade. ¡Que mujeres!: from its first
  // note, in under Peralonso's tail. Then the wind (unless --silence) rises
  // over the music and drowns it: it begins ten seconds before the last
  // sentence and swells for eight; the music holds until the wind is half up,
  // then sinks under it and is gone a second before the first word. Both
  // curves are raised cosines on the film's own clock (t, after the delay).
  const ms = (s) => Math.round(s * 1000);
  const C = seconds(cues.closingStart);
  const windFrom = C - 10.0;
  const windFull = C - 2.0;
  const sinkFrom = silence ? C - 7.5 : C - 6.0;
  const sinkTo = silence ? C - 2.5 : C - 1.0;
  const windFor = length - windFrom;
  const ease = (a, b) => `(0.5-0.5*cos(PI*min(1,max(0,(t-${a.toFixed(3)})/${(b - a).toFixed(3)}))))`;
  const inputs = ['-i', silent, '-i', PERALONSO.file, '-i', MUJERES.file];
  const parts = [
    `[1:a]atrim=start=${from.toFixed(3)}:end=${PERALONSO.to},asetpts=PTS-STARTPTS,afade=t=in:d=0.6,afade=t=out:st=${(peralonsoEnd - CROSSFADE).toFixed(3)}:d=${CROSSFADE},volume=${LEVEL}[p]`,
    `[2:a]atrim=start=${MUJERES.from}:end=${(MUJERES.from + sinkTo - mujeresAt + 0.2).toFixed(3)},asetpts=PTS-STARTPTS,afade=t=in:d=${CROSSFADE},volume=${LEVEL},adelay=${ms(mujeresAt)}|${ms(mujeresAt)},` +
      `volume='1-${ease(sinkFrom, sinkTo)}':eval=frame[m]`,
  ];
  let mix = '[p][m]';
  if (!silence) {
    inputs.push('-f', 'lavfi', '-i', `anoisesrc=color=brown:amplitude=0.9:seed=1913:duration=${windFor.toFixed(3)}`,
      '-f', 'lavfi', '-i', `anoisesrc=color=pink:amplitude=0.7:seed=1967:duration=${windFor.toFixed(3)}`);
    parts.push(
      `[3:a]highpass=f=60,lowpass=f=700[roar]`,
      `[4:a]bandpass=f=520:width_type=q:w=5,bandpass=f=520:width_type=q:w=5,aphaser=in_gain=0.6:out_gain=0.8:delay=4:decay=0.6:speed=0.12[howl]`,
      `[roar][howl]amix=inputs=2:weights=1 0.8:normalize=0,aformat=channel_layouts=stereo,adelay=${ms(windFrom)}|${ms(windFrom)},` +
        // At full the wind is as loud as the music was, and its lulls shallow
        // (never below about two thirds), so it takes the music's place rather
        // than leaving a quieter sound behind it.
        `volume='${ease(windFrom, windFull)}*(0.82+0.12*sin(2*PI*0.09*t)+0.06*sin(2*PI*0.23*t+1.3))':eval=frame,volume=3dB,` +
        `afade=t=out:st=${(length - 2.5).toFixed(3)}:d=2.5[w]`,
    );
    mix += '[w]';
  }
  // Exactly the film's length: a bed of digital silence as long as the film
  // is mixed in, so the sound neither stops early nor runs on. (A bare apad
  // pads for ever; with whole_dur it did not pad at all.)
  const bed = inputs.filter((x) => x === '-i').length;
  inputs.push('-f', 'lavfi', '-t', length.toFixed(3), '-i', 'anullsrc=channel_layout=stereo:sample_rate=44100');
  parts.push(`${mix}[${bed}:a]amix=inputs=${silence ? 3 : 4}:normalize=0:duration=longest,atrim=end=${length.toFixed(3)}[a]`);
  const mux = spawn('ffmpeg', ['-y', '-loglevel', 'error', ...inputs,
    '-filter_complex', parts.join(';'), '-map', '0:v', '-map', '[a]', '-c:v', 'copy', '-c:a', 'aac', '-b:a', '192k', '-shortest', '-movflags', '+faststart', film], { stdio: 'inherit' });
  const code = await new Promise((r) => mux.on('close', r));
  if (code !== 0) { console.error('mux failed'); process.exit(1); }
  console.log(film, `${length.toFixed(1)} s`);
}
await browser.close();
server.close();
