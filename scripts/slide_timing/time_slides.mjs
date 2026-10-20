// How long each documentation slide takes to open, and where the time goes (^ticket(470)).
//
//   node scripts/slide_timing/time_slides.mjs <devtools port> <app url> <slides.tsv> > timing.jsonl
//
// For a GitHub runner, not the laptop (headless Chrome is the load that crashes it). Each slide in a tab of its
// own, opened through dev_serve.py's /fresh (saved editor state cleared, so the shipped slide is what loads), with
// the panel closed. A slide is open at its last change to the page before 2 s with none, up to LIMIT; that time,
// from the app page's navigation, is `open_ms`. Over the same span: Chrome's Performance metrics for the tab
// (script, layout, style; the redirect page is a few ms of them), and a CPU profile of the page and of each worker
// it starts (the eval worker), sampled every 1 ms. A profile's time is split into self time by function, and
// into (garbage collector), (program) (the browser's own work outside JS) and (idle).
// One JSON line per slide.
import { readFileSync } from "node:fs";

const [dport, app, list] = process.argv.slice(2);
const LIMIT = Number(process.env.SLIDE_LIMIT_MS || 180000), QUIET = 2000;
const slides = readFileSync(list, "utf8").split("\n").filter(Boolean).map(l => l.split("\t"));
const sleep = ms => new Promise(r => setTimeout(r, ms));

const RECORDER = `(() => {
  window.__lastMut = performance.now(); window.__muts = 0;
  new MutationObserver(() => { window.__lastMut = performance.now(); window.__muts++; })
    .observe(document, { subtree: true, childList: true, attributes: true, characterData: true });
})();`;

function profileSummary(profile, top = 25) {
  if (!profile || !profile.nodes) return null;
  const byId = new Map(profile.nodes.map(n => [n.id, n]));
  const self = new Map(), frames = new Map();
  const dt = profile.timeDeltas || [];
  let total = 0;
  (profile.samples || []).forEach((id, i) => {
    const n = byId.get(id), ms = (dt[i] || 0) / 1000;
    total += ms;
    const f = n.callFrame, url = (f.url || "").split("/").pop();
    // (^ticket(470)) where each function starts in its script: phases.py maps it through the bundle's source map
    if (f.url) { const k = `${url}\t${f.lineNumber}\t${f.columnNumber}\t${f.functionName}`; frames.set(k, (frames.get(k) || 0) + ms); }
    const key = f.functionName ? `${f.functionName} ${url}` : (url ? `(anonymous) ${url}` : "(anonymous)");
    const name = ["(garbage collector)", "(program)", "(idle)", "(root)"].includes(f.functionName) ? f.functionName : key;
    self.set(name, (self.get(name) || 0) + ms);
  });
  const get = k => Math.round(self.get(k) || 0);
  const js = total - get("(garbage collector)") - get("(program)") - get("(idle)");
  return {
    total_ms: Math.round(total), js_ms: Math.round(js), gc_ms: get("(garbage collector)"),
    program_ms: get("(program)"), idle_ms: get("(idle)"),
    top: [...self].filter(([k]) => !k.startsWith("(")).sort((a, b) => b[1] - a[1]).slice(0, top)
      .map(([k, v]) => [k, Math.round(v)]),
    // [script, line, column, function, self ms], every function with at least a millisecond
    frames: [...frames].filter(([, v]) => v >= 1).map(([k, v]) => { const [u, l, c, fn] = k.split("\t"); return [u, +l, +c, fn, Math.round(v * 10) / 10]; }),
  };
}

async function timeSlide(deck, slug, name) {
  const page = await (await fetch(`http://127.0.0.1:${dport}/json/new?about:blank`, { method: "PUT" })).json();
  const ws = new WebSocket(page.webSocketDebuggerUrl);
  await new Promise(r => ws.addEventListener("open", r, { once: true }));
  let n = 0;
  const pending = new Map(), workers = new Map(), errs = [];
  const send = (method, params = {}, sessionId) => new Promise(res => {
    const id = ++n; pending.set(id, res);
    ws.send(JSON.stringify(sessionId ? { id, method, params, sessionId } : { id, method, params }));
  });
  ws.addEventListener("message", async e => {
    const m = JSON.parse(e.data);
    if (m.id && pending.has(m.id)) { pending.get(m.id)(m); pending.delete(m.id); return; }
    if (m.method === "Target.attachedToTarget") {
      const { sessionId, targetInfo } = m.params;
      // every target auto-attach finds is paused until told to run; only workers are profiled
      if (targetInfo.type === "worker" || targetInfo.type === "service_worker") {
        workers.set(sessionId, targetInfo.url);
        await send("Profiler.enable", {}, sessionId);
        await send("Profiler.setSamplingInterval", { interval: 1000 }, sessionId);
        await send("Profiler.start", {}, sessionId);
      }
      await send("Runtime.runIfWaitingForDebugger", {}, sessionId);
    }
    if (m.method === "Runtime.exceptionThrown" && !m.sessionId)
      errs.push(JSON.stringify(m.params.exceptionDetails).slice(0, 300));
  });
  const out = { deck, slug, name };
  try {
    await send("Runtime.enable");
    await send("Performance.enable");
    await send("Page.enable");
    await send("Page.addScriptToEvaluateOnNewDocument", { source: RECORDER });
    await send("Target.setAutoAttach", { autoAttach: true, waitForDebuggerOnStart: true, flatten: true });
    await send("Emulation.setDeviceMetricsOverride", { width: 1600, height: 1000, deviceScaleFactor: 1, mobile: false });
    await send("Profiler.enable");
    await send("Profiler.setSamplingInterval", { interval: 1000 });
    const before = (await send("Performance.getMetrics")).result.metrics;
    await send("Profiler.start");
    const t0 = Date.now();
    await send("Page.navigate", { url: `${app}/fresh?slide=${slug}&panel=none` });
    let st = null;
    while (Date.now() - t0 < LIMIT) {
      await sleep(250);
      const r = await send("Runtime.evaluate", {
        returnByValue: true,
        expression: `({ fresh: location.pathname.startsWith("/fresh"), slide: location.search.includes("slide="),
          editor: !!document.querySelector("#main .code-editor"), last: window.__lastMut || 0, now: performance.now(),
          muts: window.__muts || 0, timedOut: (document.body ? document.body.innerText : "").includes("Evaluation timed out") })`,
      });
      st = r.result && r.result.result && r.result.result.value;
      if (st && !st.fresh && st.editor && st.now - st.last >= QUIET) break;
    }
    const after = (await send("Performance.getMetrics")).result.metrics;
    const metric = (ms, k) => (ms.find(x => x.name === k) || { value: 0 }).value;
    const delta = k => Math.round(1000 * (metric(after, k) - metric(before, k)));
    out.open_ms = st && st.editor ? Math.round(st.last) : null;
    out.settled = !!(st && !st.fresh && st.editor && st.now - st.last >= QUIET);
    out.wall_ms = Date.now() - t0;
    out.mutations = st ? st.muts : null;
    out.timed_out_banner = !!(st && st.timedOut);
    out.metrics = { script_ms: delta("ScriptDuration"), layout_ms: delta("LayoutDuration"),
                    style_ms: delta("RecalcStyleDuration"), task_ms: delta("TaskDuration"),
                    js_heap_mb: Math.round(metric(after, "JSHeapUsedSize") / 1048576) };
    out.main = profileSummary((await send("Profiler.stop")).result.profile);
    out.workers = [];
    for (const [sid, url] of workers) {
      const r = await send("Profiler.stop", {}, sid);
      const s = profileSummary(r.result && r.result.profile);
      if (s) out.workers.push({ url: url.split("/").pop(), ...s });
    }
    out.page_errors = errs.slice(0, 5);
  } catch (e) {
    out.error = String(e).slice(0, 300);
  } finally {
    try { ws.close(); } catch (_) {}
    await fetch(`http://127.0.0.1:${dport}/json/close/${page.id}`).catch(() => {});
  }
  return out;
}

for (const [deck, slug, name] of slides) {
  const r = await timeSlide(deck, slug, name);
  console.log(JSON.stringify(r));
  console.error(`${slug}: ${r.open_ms} ms${r.settled ? "" : " (not settled)"}; main js ${r.main && r.main.js_ms}, `
    + `workers ${r.workers ? r.workers.map(w => w.js_ms).join("+") : "-"}`);
}
