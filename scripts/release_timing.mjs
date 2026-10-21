// Kids' Choice eye-size slider releases, timed in headless Chrome over its DevTools protocol (a port of
// scripts/release_timing.py, which drove Firefox on the laptop; this runs on GitHub's machines). The playground's
// Kids' Choice slide, no debug panel: each release sets the "eye size" slider (input, then change) and waits for the
// face's eye circle to redraw. The first release after load is kept apart from the rest. Prints one JSON line.
//   node release_timing.mjs <devtools port> <base url> <label> [runs]
const [dport, base, label, runsArg] = process.argv.slice(2);
const RUNS = +(runsArg || 12);
const SLIDE = "livelit-playground-emotion-kids-choice";
const VALUES = [80, 20, 70, 30, 60, 40, 90, 10, 50, 75, 25, 65];
const sleep = ms => new Promise(r => setTimeout(r, ms));

const page = await (await fetch(`http://127.0.0.1:${dport}/json/new?about:blank`, { method: "PUT" })).json();
const ws = new WebSocket(page.webSocketDebuggerUrl);
await new Promise((ok, no) => { ws.addEventListener("open", ok, { once: true }); ws.addEventListener("error", no, { once: true }); });
let n = 0; const pending = new Map(), errs = [];
ws.addEventListener("message", e => { const m = JSON.parse(e.data); if (m.id && pending.has(m.id)) { pending.get(m.id)(m); pending.delete(m.id); }
  if (m.method === "Runtime.exceptionThrown") errs.push(JSON.stringify(m.params.exceptionDetails).slice(0, 300)); });
const send = (method, params = {}) => new Promise(res => { const i = ++n; pending.set(i, res); ws.send(JSON.stringify({ id: i, method, params })); });
const ev = async x => { const r = await send("Runtime.evaluate", { expression: x, awaitPromise: true, returnByValue: true });
  if (r.result && r.result.exceptionDetails) throw new Error(JSON.stringify(r.result.exceptionDetails).slice(0, 300)); return r.result ? r.result.result.value : undefined; };

const HELPERS = `
window.__face = () => [...document.querySelectorAll('svg')].find(s => (s.getAttribute('viewBox') || '').startsWith('0 -80 200'));
window.__eyeR = () => { const f = __face(); const c = f && [...f.querySelectorAll('circle')].find(c => c.getAttribute('cy') === '100'); return c ? c.getAttribute('r') : null; };
window.__slider = label => { for (const inp of document.querySelectorAll('input[type=range]')) { const box = inp.parentElement; if (box && box.textContent.trim().startsWith(label)) return inp; } return null; };
window.__release = (label, value) => new Promise(done => {
  const inp = __slider(label); if (!inp) return done({ error: 'no slider ' + label });
  const before = __eyeR(), gaps = []; let last = performance.now(), running = true;
  const tick = () => { const now = performance.now(); if (now - last > 50) gaps.push(Math.round(now - last)); last = now; if (running) setTimeout(tick, 10); };
  setTimeout(tick, 10);
  const t0 = performance.now();
  inp.value = String(value); inp.dispatchEvent(new Event('input', { bubbles: true }));
  const tInput = performance.now() - t0;
  inp.dispatchEvent(new Event('change', { bubbles: true }));
  const tChange = performance.now() - t0 - tInput;
  const poll = () => { const after = __eyeR(), el = performance.now() - t0;
    if (after !== before || el > 120000) { running = false; done({ value, before, after, inputMs: Math.round(tInput), changeMs: Math.round(tChange), totalMs: Math.round(el), gapsMs: gaps }); }
    else setTimeout(poll, 20); };
  poll();
});
"done"`;

const out = { label, runs: [], load_ms: null, errors: errs };
try {
  await send("Runtime.enable");
  await send("Emulation.setDeviceMetricsOverride", { width: 1600, height: 1000, deviceScaleFactor: 1, mobile: false });
  const t0 = Date.now();
  await send("Page.navigate", { url: `${base}/?slide=${SLIDE}&panel=none` });
  let loaded = false;
  for (let i = 0; i < 360 && !loaded; i++) {
    await sleep(500);
    loaded = await ev(`document.querySelectorAll('input[type=range]').length > 3 && !![...document.querySelectorAll('svg')].find(s => (s.getAttribute('viewBox')||'').startsWith('0 -80 200'))`).catch(() => false);
  }
  out.load_ms = loaded ? Date.now() - t0 : null;
  if (!loaded) throw new Error("the slide did not load in 180 s");
  await sleep(5000);
  await ev(HELPERS);
  for (let i = 0; i < RUNS; i++) {
    await sleep(3000);
    out.runs.push(await ev(`__release("eye size", ${VALUES[i % VALUES.length]})`));
  }
} catch (e) { out.error = String(e).slice(0, 300); }
finally { await fetch(`http://127.0.0.1:${dport}/json/close/${page.id}`).catch(() => {}); }
const totals = out.runs.map(r => r && r.totalMs).filter(Number.isFinite);
const med = a => { const b = [...a].sort((x, y) => x - y); return b.length ? (b[(b.length - 1) >> 1] + b[b.length >> 1]) / 2 : null; };
out.first_ms = totals[0] ?? null;
out.rest_median_ms = med(totals.slice(1));
out.rest_min_ms = totals.length > 1 ? Math.min(...totals.slice(1)) : null;
out.rest_max_ms = totals.length > 1 ? Math.max(...totals.slice(1)) : null;
console.log(JSON.stringify(out));
process.exit(0);
