// MEASURING ONLY (^ticket(553)): where the map's set-up goes. Common.squares, from the game's rule texts (hazel-map-553),
// timed whole and in its parts (the order check, the count, the balanced build, and the cells' conversion from JS alone),
// as written and with the map's helpers unannotated (their ": (...)" types taken out), per size: median ms.
//   node --stack-size=8000 setup553.mjs <hazel-lib.js> <25-scripting.js> <sizes> <out.json>
import fs from "node:fs"; import vm from "node:vm"; import { createRequire } from "node:module";
const [LIB, SCRIPTING, SIZES, OUT] = process.argv.slice(2);
globalThis.require = createRequire(import.meta.url);
vm.runInThisContext(fs.readFileSync(LIB, "utf8"), { filename: "hazel-lib.js" });
const H = globalThis.Hazel;
const page = { console, Math, JSON, Array, Object, String, Number, Boolean, Map, Set, Promise, RegExp, Error, Symbol, URLSearchParams,
  location: { search: "" }, setTimeout: () => 0, clearTimeout() {}, performance,
  document: { head: { append() {} }, getElementById: () => null, body: { classList: { contains: () => false } } }, addEventListener() {},
  state: { scripts: { on: false, common: null, rooms: {} }, background: {} } };
page.window = page; page.Hazel = H; vm.createContext(page);
vm.runInContext(fs.readFileSync(SCRIPTING, "utf8") + ";globalThis.__S={SCRIPT_PREAMBLE,SCRIPT_COMMON};", page);
const S = page.__S, body = t => t.replace(/;\s*$/, "").trim();
// The map's members are Common's from its "# The world's squares as a map" line on; unannotated, each "fun ARGS : TYPE ->"
// loses its ": TYPE".
const at = S.SCRIPT_COMMON.indexOf("# The world's squares as a map");
if (at < 0) { console.error("no map in Common"); process.exit(1); }
const head = S.SCRIPT_COMMON.slice(0, at), mapText = S.SCRIPT_COMMON.slice(at);
const unann = mapText.split("\n").map(l => l.replace(/^(let \w+ = fun (?:\([^)]*\)|\w+)) : .*? ->/, "$1 ->")).join("\n");
const changed = mapText.split("\n").filter((l, i) => l !== unann.split("\n")[i]).length;
const program = common => [S.SCRIPT_PREAMBLE, `module Common = {\n${body(common)}\n} in`,
  `(squares = fun cells : [Cell] -> Common.squares(cells), in_order = fun cells : [Cell] -> Common.cells_in_order(cells), ` +
  `count = fun cells : [Cell] -> Common.count_cells(cells), take = fun cells : [Cell] -> (let (m, _) = Common.take_squares(cells, Common.count_cells(cells)) in m), ` +
  `id = fun cells : [Cell] -> 0)`].join("\n");
const V = {};
for (const [name, common] of [["annotated", head + mapText], ["unannotated", head + unann]]) {
  const r = H.run(program(common));
  if (r.errors && r.errors.length) { console.error(name, JSON.stringify(r.errors).slice(0, 1200)); process.exit(1); }
  V[name] = r.value;
}
console.log(JSON.stringify({ unannotated_lines: changed }));
const q = a => a.slice().sort((x, y) => x - y)[a.length >> 1];
const time = (f, R) => { f(); const t = []; const t00 = performance.now();
  for (let i = 0; i < R; i++) { const a = performance.now(); f(); t.push(performance.now() - a); if (performance.now() - t00 > 8000 && i >= 2) break; }
  return +q(t).toFixed(2); };
const rows = [];
for (const n of SIZES.split(",").map(Number)) {
  const cells = []; for (let row = 0; row < n; row++) for (let col = 0; col < n; col++) cells.push({ col, row, kind: "grass", ways: ["N", "E", "S", "W"] });
  const row = { n, cells: n * n };
  for (const name of ["annotated", "unannotated"]) {
    const v = V[name];
    if (v.count(cells) !== n * n || v.in_order(cells) !== true) { console.error(name, n, "wrong count or order"); process.exit(1); }
    for (const part of ["squares", "in_order", "count", "take", "id"]) row[`${name}_${part}`] = time(() => v[part](cells), n >= 32 ? 5 : 15);
  }
  console.log(JSON.stringify(row)); rows.push(row);
}
fs.writeFileSync(OUT, JSON.stringify(rows, null, 1));
