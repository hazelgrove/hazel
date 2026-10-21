// ^ticket(513): where a Kids' Choice slider release spends its time, by Hazel phase and function. Loads the playground's
// Kids' Choice slide (no debug panel), then CPU-profiles the page and the eval worker (sampled every 1 ms) over twelve
// releases of the eye-size slider only, and summarizes each profile with profile.mjs (self time by function; with
// PHASE_MAP, time charged to the nearest Hazel caller's phase, as the slide timing does). Prints one JSON line.
//   PHASE_MAP=phasemap.json node release_profile.mjs <devtools port> <base url> <label> [runs]
import { profileSummary } from "./profile.mjs";
import fs from "node:fs";
const [dport, base, label, runsArg] = process.argv.slice(2);
const RUNS = +(runsArg || 12);
const SLIDE = "livelit-playground-emotion-kids-choice";
const VALUES = [80, 20, 70, 30, 60, 40, 90, 10, 50, 75, 25, 65];
const PMAP = process.env.PHASE_MAP ? JSON.parse(fs.readFileSync(process.env.PHASE_MAP, "utf8")) : null;
const sleep = ms => new Promise(r => setTimeout(r, ms));

const page = await (await fetch(`http://127.0.0.1:${dport}/json/new?about:blank`, { method: "PUT" })).json();
const ws = new WebSocket(page.webSocketDebuggerUrl);
await new Promise((ok, no) => { ws.addEventListener("open", ok, { once: true }); ws.addEventListener("error", no, { once: true }); });
let n = 0; const pending = new Map(), errs = [], workers = new Map();
const send = (method, params = {}, sessionId) => new Promise((res, rej) => {
  const id = ++n, timer = setTimeout(() => { pending.delete(id); rej(new Error(`${method}: no answer`)); }, 120000);
  pending.set(id, m => { clearTimeout(timer); res(m); });
  ws.send(JSON.stringify(sessionId ? { id, method, params, sessionId } : { id, method, params }));
});
ws.addEventListener("message", async e => {
  const m = JSON.parse(e.data);
  if (m.id && pending.has(m.id)) { const p = pending.get(m.id); pending.delete(m.id); p(m); return; }
  if (m.method === "Target.attachedToTarget") {
    const { sessionId, targetInfo } = m.params;
    try {
      if (targetInfo.type === "worker") { workers.set(sessionId, targetInfo.url); await send("Profiler.enable", {}, sessionId); await send("Profiler.setSamplingInterval", { interval: 1000 }, sessionId); }
      await send("Runtime.runIfWaitingForDebugger", {}, sessionId);
    } catch (err) { errs.push(String(err).slice(0, 200)); }
  }
  if (m.method === "Runtime.exceptionThrown" && !m.sessionId) errs.push(JSON.stringify(m.params.exceptionDetails).slice(0, 300));
});
const ev = async x => { const r = await send("Runtime.evaluate", { expression: x, awaitPromise: true, returnByValue: true });
  if (r.result && r.result.exceptionDetails) throw new Error(JSON.stringify(r.result.exceptionDetails).slice(0, 300)); return r.result ? r.result.result.value : undefined; };

const HELPERS = `
window.__face = () => [...document.querySelectorAll('svg')].find(s => (s.getAttribute('viewBox') || '').startsWith('0 -80 200'));
window.__eyeR = () => { const f = __face(); const c = f && [...f.querySelectorAll('circle')].find(c => c.getAttribute('cy') === '100'); return c ? c.getAttribute('r') : null; };
window.__slider = label => { for (const inp of document.querySelectorAll('input[type=range]')) { const box = inp.parentElement; if (box && box.textContent.trim().startsWith(label)) return inp; } return null; };
window.__release = (label, value) => new Promise(done => {
  const inp = __slider(label); if (!inp) return done({ error: 'no slider ' + label });
  const before = __eyeR(), t0 = performance.now();
  inp.value = String(value); inp.dispatchEvent(new Event('input', { bubbles: true })); inp.dispatchEvent(new Event('change', { bubbles: true }));
  const poll = () => { const after = __eyeR(), el = performance.now() - t0; if (after !== before || el > 120000) done({ value, totalMs: Math.round(el) }); else setTimeout(poll, 10); };
  poll();
});
"done"`;

const out = { label, runs: [], errors: errs };
try {
  await send("Runtime.enable"); await send("Page.enable"); await send("Performance.enable");
  await send("Target.setAutoAttach", { autoAttach: true, waitForDebuggerOnStart: true, flatten: true });
  await send("Emulation.setDeviceMetricsOverride", { width: 1600, height: 1000, deviceScaleFactor: 1, mobile: false });
  await send("Profiler.enable"); await send("Profiler.setSamplingInterval", { interval: 1000 });
  await send("Page.navigate", { url: `${base}/fresh?slide=${SLIDE}&panel=none` });
  let loaded = false;
  for (let i = 0; i < 360 && !loaded; i++) {
    await sleep(500);
    loaded = await ev(`!location.pathname.startsWith('/fresh') && document.querySelectorAll('input[type=range]').length > 3 && !![...document.querySelectorAll('svg')].find(s => (s.getAttribute('viewBox')||'').startsWith('0 -80 200'))`).catch(() => false);
  }
  if (!loaded) throw new Error("the slide did not load in 180 s");
  await sleep(5000);
  await ev(HELPERS);
  // (one release first, outside the profiles: the first after load is a case of its own)
  out.first = await ev(`__release("eye size", 85)`);
  await sleep(2000);
  const metric = (ms, k) => (ms.find(x => x.name === k) || { value: 0 }).value;
  const m0 = (await send("Performance.getMetrics")).result.metrics;
  await send("Profiler.start");
  for (const sid of workers.keys()) await send("Profiler.start", {}, sid).catch(() => {});
  for (let i = 0; i < RUNS; i++) { out.runs.push(await ev(`__release("eye size", ${VALUES[i % VALUES.length]})`)); await sleep(1200); }
  const m1 = (await send("Performance.getMetrics")).result.metrics;
  const d = k => Math.round(1000 * (metric(m1, k) - metric(m0, k)));
  out.metrics = { script_ms: d("ScriptDuration"), layout_ms: d("LayoutDuration"), style_ms: d("RecalcStyleDuration"), task_ms: d("TaskDuration") };
  out.main = profileSummary((await send("Profiler.stop")).result.profile, PMAP, 40);
  out.workers = [];
  for (const [sid, url] of workers) {
    const r = await send("Profiler.stop", {}, sid).catch(() => null);
    const s = r && r.result && profileSummary(r.result.profile, PMAP, 40);
    if (s) out.workers.push({ url: String(url).split("/").pop(), ...s });
  }
} catch (e) { out.error = String(e).slice(0, 300); }
finally { await fetch(`http://127.0.0.1:${dport}/json/close/${page.id}`).catch(() => {}); }
const totals = out.runs.map(r => r && r.totalMs).filter(Number.isFinite).sort((a, b) => a - b);
out.median_ms = totals.length ? totals[(totals.length - 1) >> 1] : null;
console.log(JSON.stringify(out));
process.exit(0);
