// MEASURING ONLY (^ticket(553)): the World rule with the world's squares held in a map (Common.squares, World.with_squares,
// the game's own rule texts from kids-choice-games' hazel-map-553), against the rule as shipped (World.on_event, the cells
// passed with every way), in Hazel's JS library, per map size: median ms a call, answers checked.
//   node --stack-size=8000 map553.mjs <hazel-lib.js> <25-scripting.js> <sizes> <out.json>
// First a check, before any timing: on a world of random kinds and ways, its cells shuffled, the map rule and the shipped rule
// give the same answer for every square and all eight directions.
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
vm.runInContext(fs.readFileSync(SCRIPTING, "utf8") + ";globalThis.__S={SCRIPT_PREAMBLE,SCRIPT_COMMON,SCRIPT_WORLD,none:scriptNoGlobals()};", page);
const S = page.__S, body = t => t.replace(/;\s*$/, "").trim();
// The program as the game joins it (scriptProgram), with Common and World only, giving the shipped rule and the held one.
const prog = [S.SCRIPT_PREAMBLE, `module Common = {\n${body(S.SCRIPT_COMMON)}\n} in`, `module World = {\n${body(S.SCRIPT_WORLD)}\n} in`,
  `(on_event = World.on_event, held = fun cells : [Cell] -> World.with_squares(Common.squares(cells)))`].join("\n");
let t0 = performance.now();
const r = H.run(prog);
if (r.errors && r.errors.length) { console.error(JSON.stringify(r.errors).slice(0, 1500)); process.exit(1); }
const run_ms = +(performance.now() - t0).toFixed(1);
const V = r.value;
const ev = d => ({ tag: "Way", value: d });
const DIRS = ["N", "NE", "E", "SE", "S", "SW", "W", "NW"];
const globals = (n, ac, ar, cells) => ({ ...S.none, at_col: ac, at_row: ar, cols: n, rows: n, cells });

// The check: random kinds (a waterfall now and then) and ways, the cells in a shuffled order, every square, every direction.
let seed = 553; const rnd = () => (seed = (seed * 1103515245 + 12345) % 2147483648) / 2147483648;
{
  const n = 7, cells = [];
  for (let row = 0; row < n; row++) for (let col = 0; col < n; col++)
    if (rnd() < 0.9) cells.push({ col, row, kind: rnd() < 0.15 ? "waterfall" : "grass", ways: DIRS.filter(() => rnd() < 0.5) });
  for (let i = cells.length - 1; i > 0; i--) { const j = Math.floor(rnd() * (i + 1)); [cells[i], cells[j]] = [cells[j], cells[i]]; }
  const held = V.held(cells);
  let same = 0, goes = 0;
  for (let row = 0; row < n; row++) for (let col = 0; col < n; col++) for (const d of DIRS) {
    const a = JSON.stringify(V.on_event(globals(n, col, row, cells), ev(d))), b = JSON.stringify(held(globals(n, col, row, []), ev(d)));
    if (a !== b) { console.error(`check: at (${col}, ${row}) going ${d}: shipped ${a}, map ${b}`); process.exit(1); }
    same++; if (a.includes("GoSquare")) goes++;
  }
  console.log(JSON.stringify({ check: "map rule = shipped rule", cases: same, of_them_going: goes, cells: cells.length, shuffled: true, run_ms }));
}

const q = a => a.slice().sort((x, y) => x - y)[a.length >> 1];
const time = (f, R) => { for (let i = 0; i < 3; i++) f(); const t = []; const t00 = performance.now();
  for (let i = 0; i < R; i++) { const a = performance.now(); f(); t.push(performance.now() - a); if (performance.now() - t00 > 2500 && i >= 8) break; }
  return +q(t).toFixed(4); };
const rows = [];
for (const n of SIZES.split(",").map(Number)) {
  // as report 25 and 27: every square grass with four ways, the cells in the game's order (row, then column), mid-map, going east
  const cells = []; for (let row = 0; row < n; row++) for (let col = 0; col < n; col++) cells.push({ col, row, kind: "grass", ways: ["N", "E", "S", "W"] });
  const ac = Math.floor(n / 2) - 1, ar = Math.floor(n / 2), e = ev("E");
  const want = JSON.stringify({ tag: "GoSquare", value: [ac + 1, ar, "W", { tag: "Done" }] });
  const setups = []; let held;
  for (let i = 0; i < 3; i++) { const a = performance.now(); held = V.held(cells); setups.push(performance.now() - a); }
  const handle = typeof H.value === "function" ? H.value(cells) : null;
  const g = globals(n, ac, ar, cells), small = globals(n, ac, ar, []), gh = handle ? globals(n, ac, ar, handle) : null;
  const checks = [["passed", () => V.on_event(g, e)], ["map", () => held(small, e)], ...(gh ? [["passed_handle", () => V.on_event(gh, e)]] : [])];
  for (const [k, f] of checks) if (JSON.stringify(f()) !== want) { console.error(k, n, JSON.stringify(f()), "wanted", want); process.exit(1); }
  const R = n >= 32 ? 20 : 60;
  const row = { n, cells: n * n, map_setup: +q(setups).toFixed(2) };
  for (const [k, f] of checks) row[k] = time(f, R);
  console.log(JSON.stringify(row)); rows.push(row);
}
fs.writeFileSync(OUT, JSON.stringify(rows, null, 1));
