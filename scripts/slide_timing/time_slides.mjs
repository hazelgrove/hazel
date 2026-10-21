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
// With PHASE_MAP set (phases.py --emit-map), each profile's time is also charged to the nearest Hazel caller
// (profile.mjs).
// One JSON line per slide.
import { readFileSync } from "node:fs";
import { profileSummary } from "./profile.mjs";

const [dport, app, list] = process.argv.slice(2);
const LIMIT = Number(process.env.SLIDE_LIMIT_MS || 180000), QUIET = 2000;
// Every DevTools call answers or fails: one with no answer in CALL_MS (or, while polling a slide, in what is left of
// LIMIT), or whose socket closes, is rejected, so a stuck slide is recorded with its error and the next slide still
// runs. #2597's run 39482019681 lost every slide after Witch: a call there never answered, and node exited 13 on a
// top-level await that never settled.
const CALL_MS = Number(process.env.SLIDE_CALL_MS || 60000), NET_MS = 30000;
const slides = readFileSync(list, "utf8").split("\n").filter(Boolean).map(l => l.split("\t"));
const sleep = ms => new Promise(r => setTimeout(r, ms));
// which phase each function is in (phases.py --emit-map), to charge library time to the nearest Hazel caller
const PMAP = process.env.PHASE_MAP ? JSON.parse(readFileSync(process.env.PHASE_MAP, "utf8")) : null;

const RECORDER = `(() => {
  window.__lastMut = performance.now(); window.__muts = 0;
  new MutationObserver(() => { window.__lastMut = performance.now(); window.__muts++; })
    .observe(document, { subtree: true, childList: true, attributes: true, characterData: true });
})();`;

async function timeSlide(deck, slug, name) {
  const out = { deck, slug, name };
  let page;
  try {
    page = await (await fetch(`http://127.0.0.1:${dport}/json/new?about:blank`, { method: "PUT", signal: AbortSignal.timeout(NET_MS) })).json();
  } catch (e) { out.error = `no new tab: ${String(e).slice(0, 200)}`; return out; }
  const ws = new WebSocket(page.webSocketDebuggerUrl);
  // (open, or fail as soon as it closes or errs, or in NET_MS)
  const opened = new Promise((r, j) => {
    const tm = setTimeout(() => j(new Error(`the DevTools socket did not open in ${NET_MS} ms`)), NET_MS);
    ws.addEventListener("open", () => { clearTimeout(tm); r(); }, { once: true });
    for (const ev of ["close", "error"]) ws.addEventListener(ev, () => { clearTimeout(tm); j(new Error(`the DevTools socket ${ev === "close" ? "closed" : "failed"} before it opened`)); }, { once: true });
  });
  opened.catch(() => {});
  let n = 0, closed = false;
  const pending = new Map(), workers = new Map(), errs = [];
  const fail = why => { closed = true; for (const p of pending.values()) p.rej(new Error(why)); pending.clear(); };
  ws.addEventListener("close", () => fail("the DevTools socket closed"));
  ws.addEventListener("error", () => fail("the DevTools socket failed"));
  const send = (method, params = {}, sessionId, ms = CALL_MS) => new Promise((res, rej) => {
    if (closed) return rej(new Error(`${method}: the DevTools socket is closed`));
    const id = ++n, timer = setTimeout(() => { pending.delete(id); rej(new Error(`${method}: no answer in ${ms} ms`)); }, ms);
    pending.set(id, { res: m => { clearTimeout(timer); res(m); }, rej: e => { clearTimeout(timer); rej(e); } });
    ws.send(JSON.stringify(sessionId ? { id, method, params, sessionId } : { id, method, params }));
  });
  ws.addEventListener("message", async e => {
    const m = JSON.parse(e.data);
    if (m.id && pending.has(m.id)) { const p = pending.get(m.id); pending.delete(m.id); p.res(m); return; }
    if (m.method === "Target.attachedToTarget") {
      const { sessionId, targetInfo } = m.params;
      // every target auto-attach finds is paused until told to run; only workers are profiled (a failure here is
      // the slide's, recorded with it, not a crash of the whole run)
      try {
        if (targetInfo.type === "worker" || targetInfo.type === "service_worker") {
          workers.set(sessionId, targetInfo.url);
          await send("Profiler.enable", {}, sessionId);
          await send("Profiler.setSamplingInterval", { interval: 1000 }, sessionId);
          await send("Profiler.start", {}, sessionId);
        }
        await send("Runtime.runIfWaitingForDebugger", {}, sessionId);
      } catch (err) { errs.push(`worker ${targetInfo.type}: ${String(err).slice(0, 200)}`); }
    }
    if (m.method === "Runtime.exceptionThrown" && !m.sessionId)
      errs.push(JSON.stringify(m.params.exceptionDetails).slice(0, 300));
  });
  try {
    await opened;
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
      // (the page answers only between its tasks: a slide that keeps the main thread past LIMIT is stuck, and its
      // metrics and profiles would not come either)
      const r = await send("Runtime.evaluate", {
        returnByValue: true,
        expression: `({ fresh: location.pathname.startsWith("/fresh"), slide: location.search.includes("slide="),
          editor: !!document.querySelector("#main .code-editor"), last: window.__lastMut || 0, now: performance.now(),
          muts: window.__muts || 0, timedOut: (document.body ? document.body.innerText : "").includes("Evaluation timed out") })`,
      }, undefined, Math.max(5000, LIMIT - (Date.now() - t0) + 5000)).catch(e => { out.stuck = true; throw e; });
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
    out.main = profileSummary((await send("Profiler.stop")).result.profile, PMAP);
    out.workers = [];
    for (const [sid, url] of workers) {
      const r = await send("Profiler.stop", {}, sid);
      const s = profileSummary(r.result && r.result.profile, PMAP);
      if (s) out.workers.push({ url: url.split("/").pop(), ...s });
    }
    out.page_errors = errs.slice(0, 5);
  } catch (e) {
    out.error = String(e).slice(0, 300);
    out.wall_ms = out.wall_ms ?? null;
    out.page_errors = errs.slice(0, 5);
  } finally {
    try { ws.close(); } catch (_) {}
    await fetch(`http://127.0.0.1:${dport}/json/close/${page.id}`, { signal: AbortSignal.timeout(NET_MS) }).catch(() => {});
  }
  return out;
}

// (each slide's outcome is printed, error or not; then out, whatever a stuck tab's socket is still doing)
for (const [deck, slug, name] of slides) {
  const r = await timeSlide(deck, slug, name);
  console.log(JSON.stringify(r));
  console.error(r.error ? `${slug}: ERROR ${r.error}${r.stuck ? " (stuck: the main thread busy past the limit)" : ""}`
    : `${slug}: ${r.open_ms} ms${r.settled ? "" : " (not settled)"}; main js ${r.main && r.main.js_ms}, `
    + `workers ${r.workers ? r.workers.map(w => w.js_ms).join("+") : "-"}`);
}
process.exit(0);
