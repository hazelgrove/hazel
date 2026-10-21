// ^ticket(513): where does a Kids' Choice slider release spend its time, on a deployed (minified) build? Per release:
// the main thread's own accounting (CDP Performance.getMetrics deltas: script, layout, style, task), every message
// posted to the eval worker (count, size when a string, time to post) and every message back (count, size, when),
// and the page's long tasks. Twelve releases after a first one; prints one JSON line with the per-release records.
//   node release_probe.mjs <devtools port> <base url> <label> [runs]
const [dport, base, label, runsArg] = process.argv.slice(2);
const RUNS = +(runsArg || 12);
const SLIDE = "livelit-playground-emotion-kids-choice";
const VALUES = [80, 20, 70, 30, 60, 40, 90, 10, 50, 75, 25, 65, 35];
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
const metrics = async () => Object.fromEntries((await send("Performance.getMetrics")).result.metrics.map(m => [m.name, m.value]));

// (before the page's own scripts: watch every worker it makes, and the long tasks)
const HOOK = `
window.__w = { posts: [], msgs: [], long: [] };
(() => {
  const W0 = window.Worker; if (!W0) return;
  const sizeOf = m => typeof m === 'string' ? m.length : (m && typeof m.byteLength === 'number') ? m.byteLength : (Array.isArray(m) ? -2 : -1);
  function W(...a) { const w = new W0(...a); const pm = w.postMessage.bind(w);
    w.postMessage = (m, ...r) => { const t = performance.now(); pm(m, ...r); __w.posts.push({ t, dt: performance.now() - t, size: sizeOf(m), kind: typeof m }); };
    w.addEventListener('message', e => __w.msgs.push({ t: performance.now(), size: sizeOf(e.data), kind: typeof e.data }));
    return w; }
  W.prototype = W0.prototype; window.Worker = W;
  try { new PerformanceObserver(l => { for (const e of l.getEntries()) __w.long.push({ t: e.startTime, d: e.duration }); }).observe({ type: 'longtask', buffered: true }); } catch (e) {}
})();`;

const HELPERS = `
window.__face = () => [...document.querySelectorAll('svg')].find(s => (s.getAttribute('viewBox') || '').startsWith('0 -80 200'));
window.__eyeR = () => { const f = __face(); const c = f && [...f.querySelectorAll('circle')].find(c => c.getAttribute('cy') === '100'); return c ? c.getAttribute('r') : null; };
window.__slider = label => { for (const inp of document.querySelectorAll('input[type=range]')) { const box = inp.parentElement; if (box && box.textContent.trim().startsWith(label)) return inp; } return null; };
window.__release = (label, value) => new Promise(done => {
  const inp = __slider(label); if (!inp) return done({ error: 'no slider ' + label });
  __w.posts = []; __w.msgs = []; __w.long = [];
  const before = __eyeR(), t0 = performance.now();
  inp.value = String(value); inp.dispatchEvent(new Event('input', { bubbles: true }));
  inp.dispatchEvent(new Event('change', { bubbles: true }));
  const tEvents = performance.now() - t0;
  const poll = () => { const after = __eyeR(), el = performance.now() - t0;
    if (after !== before || el > 120000) {
      const redraw = el;
      // (a little longer, for work that trails the redraw)
      setTimeout(() => {
        const rel = x => Math.round(x - t0);
        done({ value, totalMs: Math.round(redraw), eventsMs: Math.round(tEvents),
          posts: __w.posts.length, postBytes: __w.posts.reduce((s, p) => s + Math.max(0, p.size), 0), postMs: Math.round(__w.posts.reduce((s, p) => s + p.dt, 0)),
          postKinds: [...new Set(__w.posts.map(p => p.kind))], firstPost: __w.posts.length ? rel(__w.posts[0].t) : null,
          msgs: __w.msgs.length, msgBytes: __w.msgs.reduce((s, m) => s + Math.max(0, m.size), 0), firstMsg: __w.msgs.length ? rel(__w.msgs[0].t) : null,
          lastMsgBeforeRedraw: (() => { const b = __w.msgs.filter(m => m.t - t0 <= redraw); return b.length ? rel(b[b.length - 1].t) : null; })(),
          longTasks: __w.long.filter(l => l.t >= t0).length, longMs: Math.round(__w.long.filter(l => l.t >= t0).reduce((s, l) => s + l.d, 0)),
          longMax: Math.round(Math.max(0, ...__w.long.filter(l => l.t >= t0).map(l => l.d))) });
      }, 1500);
    } else setTimeout(poll, 10); };
  poll();
});
"done"`;

const out = { label, runs: [], load_ms: null, errors: errs };
try {
  await send("Runtime.enable"); await send("Page.enable"); await send("Performance.enable", { timeDomain: "threadTicks" });
  await send("Page.addScriptToEvaluateOnNewDocument", { source: HOOK });
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
  for (let i = 0; i <= RUNS; i++) {
    await sleep(2500);
    const m0 = await metrics();
    const r = await ev(`__release("eye size", ${VALUES[i % VALUES.length]})`);
    const m1 = await metrics();
    const d = k => Math.round(1000 * ((m1[k] || 0) - (m0[k] || 0)));
    Object.assign(r, { scriptMs: d("ScriptDuration"), layoutMs: d("LayoutDuration"), styleMs: d("RecalcStyleDuration"), taskMs: d("TaskDuration"), layouts: (m1.LayoutCount || 0) - (m0.LayoutCount || 0), styles: (m1.RecalcStyleCount || 0) - (m0.RecalcStyleCount || 0), first: i === 0 });
    out.runs.push(r);
  }
} catch (e) { out.error = String(e).slice(0, 300); }
finally { await fetch(`http://127.0.0.1:${dport}/json/close/${page.id}`).catch(() => {}); }
console.log(JSON.stringify(out));
process.exit(0);
