// A CPU profile, summarized for the slide timing (^ticket(470)): self time by function, and the same time charged to
// the nearest Hazel caller.
//
// Self time alone puts most of every slide in "runtime" (js_of_ocaml's runtime and the OCaml libraries), which says
// nothing about who called it. So, given the phase map phases.py --emit-map makes from the bundles' source maps
// (which phase each generated line and column is in), each sample's time is charged to the first function on its
// stack, from the sampled one up, that is in a phase of Hazel's own: the runtime, the libraries, src/util, unmapped
// code and the browser's builtins pass their time up to their caller.

const PASS = new Set(["runtime", "util", "unmapped"]);
const SPECIAL = ["(garbage collector)", "(program)", "(idle)", "(root)"];

const bundleOf = url => (url || "").split("?")[0].split("/").pop();

// The phase of a call frame's function, from the map: null for a builtin or a script the map does not cover.
export function phaseAt(pmap, f) {
  if (!pmap || !f.url) return null;
  const lines = pmap.bundles[bundleOf(f.url)];
  if (!lines) return null;
  const segs = lines[f.lineNumber];
  if (!segs || !segs.length) return "unmapped";
  // the last run starting at or before the column, as phases.py's SourceMap.source finds it
  let lo = 0, hi = segs.length - 1, k = -1;
  while (lo <= hi) {
    const mid = (lo + hi) >> 1;
    if (segs[mid][0] <= f.columnNumber) { k = mid; lo = mid + 1; } else hi = mid - 1;
  }
  return pmap.phases[segs[k >= 0 ? k : 0][1]];
}

export function profileSummary(profile, pmap = null, top = 25) {
  if (!profile || !profile.nodes) return null;
  const byId = new Map(profile.nodes.map(n => [n.id, n]));
  const parent = new Map();
  for (const n of profile.nodes) for (const c of n.children || []) parent.set(c, n.id);
  const self = new Map(), frames = new Map();
  const dt = profile.timeDeltas || [];
  let total = 0;

  // node id -> [phase, id of the node charged]: the node itself when its phase is Hazel's, else the nearest such
  // caller; with none on the stack, the node's own phase ("browser" for a builtin), charged to itself
  const charge = new Map(), noCaller = new Set();
  const chargeOf = id => {
    const path = [];
    let cur = id, found = null;
    while (cur !== undefined && !noCaller.has(cur)) {
      if (charge.has(cur)) { found = charge.get(cur); break; }
      path.push(cur);
      const ph = phaseAt(pmap, byId.get(cur).callFrame);
      if (ph && !PASS.has(ph)) { found = [ph, cur]; break; }
      cur = parent.get(cur);
    }
    if (!found) {
      for (const p of path) noCaller.add(p);
      return [phaseAt(pmap, byId.get(id).callFrame) || "browser", id];
    }
    for (const p of path) charge.set(p, found);
    return found;
  };
  const charged = new Map(), callers = new Map();

  (profile.samples || []).forEach((id, i) => {
    const n = byId.get(id), ms = (dt[i] || 0) / 1000;
    total += ms;
    const f = n.callFrame, url = (f.url || "").split("/").pop();
    // where each function starts in its script: phases.py maps it through the bundle's source map
    if (f.url) { const k = `${url}\t${f.lineNumber}\t${f.columnNumber}\t${f.functionName}`; frames.set(k, (frames.get(k) || 0) + ms); }
    const key = f.functionName ? `${f.functionName} ${url}` : (url ? `(anonymous) ${url}` : "(anonymous)");
    const special = SPECIAL.includes(f.functionName);
    const name = special ? f.functionName : key;
    self.set(name, (self.get(name) || 0) + ms);
    if (pmap && !special) {
      const [ph, by] = chargeOf(id);
      charged.set(ph, (charged.get(ph) || 0) + ms);
      if (by !== id) {
        // library time a Hazel function was charged with: who calls the runtime
        const c = byId.get(by).callFrame;
        const k = `${bundleOf(c.url)}\t${c.lineNumber}\t${c.columnNumber}\t${c.functionName}\t${ph}`;
        callers.set(k, (callers.get(k) || 0) + ms);
      }
    }
  });
  const get = k => Math.round(self.get(k) || 0);
  const js = total - get("(garbage collector)") - get("(program)") - get("(idle)");
  const out = {
    total_ms: Math.round(total), js_ms: Math.round(js), gc_ms: get("(garbage collector)"),
    program_ms: get("(program)"), idle_ms: get("(idle)"),
    top: [...self].filter(([k]) => !k.startsWith("(")).sort((a, b) => b[1] - a[1]).slice(0, top)
      .map(([k, v]) => [k, Math.round(v)]),
    // [script, line, column, function, self ms], every function with at least a millisecond
    frames: [...frames].filter(([, v]) => v >= 1).map(([k, v]) => { const [u, l, c, fn] = k.split("\t"); return [u, +l, +c, fn, Math.round(v * 10) / 10]; }),
  };
  if (pmap) {
    // the JS time (no garbage collector, (program) or idle) by the phase it is charged to
    out.charged_ms = Object.fromEntries([...charged].map(([k, v]) => [k, Math.round(v)]));
    // [script, line, column, function, phase, ms]: the Hazel functions charged with the most library time
    out.lib_callers = [...callers].sort((a, b) => b[1] - a[1]).slice(0, 40)
      .map(([k, v]) => { const [u, l, c, fn, ph] = k.split("\t"); return [u, +l, +c, fn, ph, Math.round(v * 10) / 10]; });
  }
  return out;
}
