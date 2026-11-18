// The demo's data: which module of a library imports which, from the
// compiler's own output.
//
//   node force-downhill/extract-imports.mjs     (run by `make sync-imports`)
//
// A module is one of the library's if a file under its src/ declares it; an
// import is taken from that module's corefn.json (`imports`: the modules the
// compiled code refers to), and kept if it is another of the library's
// modules. Build the library first (`spago build` in it) so that its output/
// is current.
import { readFileSync, readdirSync, writeFileSync, existsSync, mkdirSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';

const here = dirname(fileURLToPath(import.meta.url));
const library = 'purescript-hylograph-selection';
const root = join(here, '../..', library);

const sources = (directory) => readdirSync(directory, { withFileTypes: true }).flatMap((entry) =>
  entry.isDirectory() ? sources(join(directory, entry.name)) : entry.name.endsWith('.purs') ? [join(directory, entry.name)] : []);
const modules = sources(join(root, 'src'))
  .map((file) => /^module\s+([\w.]+)/m.exec(readFileSync(file, 'utf8')))
  .filter(Boolean).map((match) => match[1]).sort();
const own = new Set(modules);

const imports = [];
for (const module of modules) {
  const corefn = join(root, 'output', module, 'corefn.json');
  if (!existsSync(corefn)) throw new Error(`no compiler output for ${module}: build ${library} first`);
  const imported = JSON.parse(readFileSync(corefn, 'utf8')).imports.map((entry) => entry.moduleName.join('.'));
  for (const other of [...new Set(imported)].sort()) if (other !== module && own.has(other)) imports.push({ from: module, to: other });
}

mkdirSync(join(here, 'public/data'), { recursive: true });
writeFileSync(join(here, 'public/data/imports.json'), JSON.stringify({ library, modules, imports }, null, 1));
console.log(`${library}: ${modules.length} modules, ${imports.length} imports between them`);
