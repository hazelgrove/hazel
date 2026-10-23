// ^ticket(542): the World rule in Fumola (wasm, node), per map size: passed (the whole map as a call's text), passed neighbours
// only, held list, held map. Each call is fumola_eval_top of a call's source text; its answer checked; median ms and Fumola's steps.
//   node measure.mjs <bindings dir> <world.fum> <sizes> <out.json>
import fs from "node:fs";
import { heldSetup } from "./held.fum.js";
const [dir, src, sizesArg, out] = process.argv.slice(2);
const F = await import(dir + "/fumola_wasm.js");
F.initSync({ module: fs.readFileSync(dir + "/fumola_wasm_bg.wasm") });
const cmd = v => v.tag === "Variant" ? (v.value.value === null ? { tag: v.value.name } : { tag: v.value.name, value: cmd(v.value.value) })
  : v.tag === "Tuple" || v.tag === "List" ? v.value.map(cmd) : v.tag === "Int" ? +v.value : v.value;
const cellText = (c, r) => `{ col = ${c}; row = ${r}; kind = "grass"; ways = ["N", "E", "S", "W"] }`;
const q = a => a.slice().sort((x, y) => x - y)[a.length >> 1];
const rows = [];
for (const n of sizesArg.split(",").map(Number)) {
  const id = F.fumola_create();
  const run = t => { const r = JSON.parse(F.fumola_eval_top(id, t)); if (!r.ok) throw new Error(JSON.stringify(r).slice(0, 300)); return r; };
  run(fs.readFileSync(src, "utf8"));
  const ac = Math.floor(n / 2) - 1, ar = Math.floor(n / 2);
  const all = []; for (let r = 0; r < n; r++) for (let c = 0; c < n; c++) all.push(cellText(c, r));
  const near = []; for (let r = 0; r < n; r++) for (let c = 0; c < n; c++) if (Math.abs(c - ac) <= 1 && Math.abs(r - ar) <= 1) near.push(cellText(c, r));
  let t0 = performance.now(); run(heldSetup("[" + all.join(", ") + "]")); const setup = performance.now() - t0;
  const want = JSON.stringify({ tag: "GoSquare", value: [ac + 1, ar, "W", { tag: "Done" }] });
  const variants = {
    passed: () => `world_list({ at_col = ${ac}; at_row = ${ar}; cols = ${n}; rows = ${n}; cells = [${all.join(", ")}] }, #Way("E"))`,
    near: () => `world_list({ at_col = ${ac}; at_row = ${ar}; cols = ${n}; rows = ${n}; cells = [${near.join(", ")}] }, #Way("E"))`,
    held_list: () => `world_held_list(${ac}, ${ar}, ${n}, ${n}, "E")`,
    held_map: () => `world_held_map(${ac}, ${ar}, ${n}, ${n}, "E")`,
  };
  const row = { n, cells: n * n, setup_ms: +setup.toFixed(2) };
  for (const [k, mk] of Object.entries(variants)) {
    for (let i = 0; i < 3; i++) run(mk());
    const ts = [], st = []; const tStart = performance.now(); let got;
    for (let i = 0; i < 60; i++) { const t = performance.now(); const r = run(mk()); ts.push(performance.now() - t); st.push(r.counts.step); got = JSON.stringify(cmd(r)); if (performance.now() - tStart > 2500 && i >= 8) break; }
    if (got !== want) throw new Error(`${k} at ${n}: ${got}, wanted ${want}`);
    row[k] = +q(ts).toFixed(3); row[k + "_steps"] = q(st);
  }
  // the source text a passed call builds and parses: its length
  row.passed_chars = variants.passed().length;
  F.fumola_drop(id);
  console.log(JSON.stringify(row)); rows.push(row);
}
fs.writeFileSync(out, JSON.stringify(rows, null, 1));
