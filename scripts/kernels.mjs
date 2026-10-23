// ^ticket(545): the same small programs in Hazel (js) and Fumola (wasm, or native through the same entry point elsewhere):
// median ms per call, and steps per call (Fumola's counts.step; Hazel's only on a build with Hazel.stepCounts()).
//   node --stack-size=8000 kernels.mjs hazel <hazel-lib.js> <out.json>
//   node kernels.mjs fumola <bindings dir> <out.json>
import fs from "node:fs"; import vm from "node:vm"; import path from "node:path"; import { createRequire } from "node:module";
const HERE = path.dirname(new URL(import.meta.url).pathname);
const [sys, where, out] = process.argv.slice(2);
const PLAN = [["count", [100, 1000, 5000]], ["fib", [10, 13, 16]], ["sumbuild", [100, 1000, 3000]], ["mapinc", [100, 1000, 3000]],
  ["skip", [100, 1000, 3000]], ["h_id", [100, 1000, 3000]], ["h_ref", [100, 1000, 3000]], ["h_walk", [100, 1000, 3000]]];
// KERNELS=h_ref,h_walk runs only those
const ONLY = process.env.KERNELS ? process.env.KERNELS.split(",") : null;
if (ONLY) PLAN.splice(0, PLAN.length, ...PLAN.filter(([k]) => ONLY.includes(k)));
const q = a => a.slice().sort((x, y) => x - y)[a.length >> 1];
const time = f => { for (let i = 0; i < 3; i++) f(); const ts = [], t00 = performance.now();
  for (let i = 0; i < 60; i++) { const t = performance.now(); f(); ts.push(performance.now() - t); if (performance.now() - t00 > 2500 && i >= 4) break; }
  return { ms: +q(ts).toFixed(4), reps: ts.length }; };
const ints = n => Array.from({ length: n }, (_, i) => i + 1);
const rows = [];
if (sys === "hazel") {
  globalThis.require = createRequire(import.meta.url);
  vm.runInThisContext(fs.readFileSync(where, "utf8"), { filename: "hazel-lib.js" });
  const H = globalThis.Hazel, counts = typeof H.stepCounts === "function";
  const r = H.run(fs.readFileSync(path.join(HERE, "kernels.hz"), "utf8"));
  if (r.errors && r.errors.length) { console.error(JSON.stringify(r.errors).slice(0, 800)); process.exit(1); }
  const V = r.value;
  for (const [k, ns] of PLAN) for (const n of ns) {
    let call, want;
    if (k.startsWith("h_")) { const h = V.hold(n); call = () => h[k](7); want = k === "h_walk" ? n : 7; }
    else if (k === "skip") { const xs = ints(n); call = () => V.skip(xs); want = 0; }
    else { call = () => V[k](n); want = { count: 0, fib: [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987][n], sumbuild: n * (n + 1) / 2, mapinc: n }[k]; }
    const got = call(); if (got !== want) throw new Error(`${k}(${n}): ${JSON.stringify(got)}, wanted ${want}`);
    const row = { sys: "hazel", k, n, ...time(call) };
    if (counts) { H.stepCounts(); call(); const c = H.stepCounts(); row.steps = Object.values(c).reduce((a, b) => a + b, 0); row.by_kind = c; }
    console.log(JSON.stringify({ ...row, by_kind: undefined })); rows.push(row);
  }
} else {
  const F = await import(where + "/fumola_wasm.js");
  F.initSync({ module: fs.readFileSync(where + "/fumola_wasm_bg.wasm") });
  const id = F.fumola_create();
  const run = t => { const r = JSON.parse(F.fumola_eval_top(id, t)); if (!r.ok) throw new Error(t.slice(0, 80) + ": " + JSON.stringify(r).slice(0, 300)); return r; };
  run(fs.readFileSync(path.join(HERE, "kernels.fum"), "utf8"));
  for (const [k, ns] of PLAN) for (const n of ns) {
    let text;
    if (k.startsWith("h_")) {
      // one text, ending in 0: an answer that is the held list itself overflows the wasm stack from about 1,000 deep (^ticket(545))
      run(`let held = build(${n}, null); func h_id(k) { k }; func h_ref(k) { let u = held; k }; func h_walk(k) { len(held) }; 0`);
      text = `${k}(7)`;
    } else if (k === "skip") text = `skip([${ints(n).join(", ")}])`;
    else text = `${k}(${n})`;
    let r; const row = { sys: "fumola", k, n, ...time(() => { r = run(text); }) };
    row.steps = r.counts.step; row.text_chars = text.length;
    console.log(JSON.stringify(row)); rows.push(row);
  }
}
fs.writeFileSync(out, JSON.stringify(rows, null, 1));
