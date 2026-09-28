// Renders the notebook from D (embedded by build.js). Shared pieces (tooltip,
// highlighter, bar chart, timeline squares, chips) are the Eval 002 report's
// functions, generalised so every tab uses the same ones.
const $ = (id) => document.getElementById(id);
const esc = (s) => String(s ?? "").replace(/[&<>"]/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" }[c]));
const usd = (x, d = 4) => (x == null ? "–" : "$" + Number(x).toFixed(d));
const pct = (x) => (x == null ? "–" : Math.round(x * 100) + "%");
const tip = $("tip");
function hoverable(el, html) {
  el.addEventListener("mousemove", (e) => { tip.innerHTML = html; tip.hidden = false; tip.style.left = Math.min(e.clientX + 14, innerWidth - 300) + "px"; tip.style.top = (e.clientY + 14) + "px"; });
  el.addEventListener("mouseleave", () => { tip.hidden = true; });
}
function hl(src) {
  return esc(src)
    .replace(/\b(let|in|fun|if|then|else|test|end|case|module|type)\b/g, '<span class="k">$1</span>')
    .replace(/\b(Int|Bool|String|Float)\b/g, '<span class="t">$1</span>')
    .replace(/(?<![\w#.])(\d+(?:\.\d+)?)\b/g, '<span class="d">$1</span>');
}
const tiles = (items) => items.map(([v, k]) => `<div class="cost-tile"><span class="v">${v}</span><span class="k">${k}</span></div>`).join("");
const goalPill = (g) => g == null ? `<span class="pill neutral">no goal</span>` : `<span class="pill ${g ? "good" : "bad"}">${g ? "✓ correct output" : "✗ wrong output"}</span>`;

// ---------- lookups ----------
const MODES = Object.fromEntries(D.modes.map((m) => [m.id, m]));
const TASKS = Object.fromEntries(D.tasks.map((t) => [t.id, t]));
const EVALS = Object.fromEntries(D.evals.map((e) => [e.id, e]));
const modeName = (id) => (MODES[id] ? MODES[id].short : id);
const taskPlain = (id) => {
  const t = TASKS[id];
  if (!t || !t.name) return id;
  return `${t.name} (${t.lines} lines, ${t.bugs} bug${t.bugs === 1 ? "" : "s"})`;
};
const runs = D.runs.map((r, i) => ({ ...r, i, jev: (r.cost_nav || 0) + (r.cost_edit || 0), total: r.cost_main == null ? null : r.cost_main + (r.cost_nav || 0) + (r.cost_edit || 0) }));
const runLabel = (r) => `Eval ${r.eval} · ${TASKS[r.task] ? TASKS[r.task].name : r.task} · ${modeName(r.mode)}`;
const sum = (xs) => xs.reduce((a, x) => a + (x || 0), 0);
const avg = (xs) => (xs.length ? sum(xs) / xs.length : null);
const byMode = (id) => runs.filter((r) => r.mode === id);

// ---------- tabs (buttons; #anchors deep-link) ----------
function showTab(id) {
  if (!$(id) || !$(id).classList.contains("nb-panel")) id = "overview";
  document.querySelectorAll(".nb-panel").forEach((p) => { p.hidden = p.id !== id; });
  $("tabs").querySelectorAll("button").forEach((b) => b.setAttribute("aria-selected", String(b.dataset.tab === id)));
}
$("tabs").querySelectorAll("button").forEach((b) => b.addEventListener("click", () => { showTab(b.dataset.tab); history.replaceState(null, "", "#" + b.dataset.tab); }));
addEventListener("hashchange", () => showTab(location.hash.slice(1)));

// ---------- shared charts ----------
// Grouped horizontal stacked bars (from the Eval 002 report).
function hbars(el, rowsIn, groupOf, labelOf, valueParts, fmt) {
  const max = Math.max(1e-9, ...rowsIn.map((r) => sum(valueParts(r).map((p) => p[1]))));
  const W = 560, L = 170, R = 76, bh = 18, gapIn = 7, gapGroup = 20;
  let y = 12, s = "", last = null; const marks = [];
  const x = (v) => L + (v / max) * (W - L - R);
  for (const r of rowsIn) {
    const g = groupOf(r);
    if (g !== last) { if (last !== null) y += gapGroup - gapIn; if (g) s += `<text x="10" y="${y + 13}" font-size="11.5" font-family="var(--mono)" font-weight="600" fill="var(--muted)">${esc(g)}</text>`; if (g) y += 18; last = g; }
    s += `<text x="${L - 8}" y="${y + bh / 2 + 4}" font-size="12" fill="var(--ink-2)" text-anchor="end">${esc(labelOf(r))}</text>`;
    let acc = 0;
    valueParts(r).forEach(([name, v, col], j) => { if (!(v > 0)) return; const x0 = x(acc) + (j ? 1 : 0); const w = Math.max(x(acc + v) - x(acc) - (j ? 1 : 0), 2); marks.push(`<rect class="hb" data-t="${esc(g)} · ${esc(labelOf(r))} · ${esc(name)}: ${esc(fmt(v))}" x="${x0}" y="${y}" width="${w}" height="${bh}" rx="3" fill="var(${col})"/>`); acc += v; });
    s += `<text x="${x(acc) + 6}" y="${y + bh / 2 + 4}" font-size="11.5" font-family="var(--mono)" fill="var(--ink)">${esc(fmt(acc))}</text>`;
    y += bh + gapIn;
  }
  el.innerHTML = rowsIn.length ? `<svg viewBox="0 0 ${W} ${y + 6}" width="100%" role="img">${marks.join("")}${s}</svg>` : `<p class="nb-empty">No runs yet.</p>`;
  el.querySelectorAll(".hb").forEach((m) => hoverable(m, m.dataset.t));
}
const costParts = (r) => [["Agent", r.cost_main || 0, "--s1"], ["Jev navigation", r.cost_nav || 0, "--s2"], ["Jev editing", r.cost_edit || 0, "--s3"]];
const cat = (n) => (n === "expand" || n === "collapse" || n === "modify_view") ? ["N", "--s2"] : n.includes("probe") ? ["P", "--s1"] : (n.startsWith("update_") || n.startsWith("insert_") || n.startsWith("delete_") || n === "jev_edit" || n === "add_tests") ? ["E", "--s3"] : ["O", "--s4"];
function timelineCells(steps) {
  return steps.map((st, i) => { const [c, col] = cat(st.name); return `<span class="cell${st.ok === false ? " fail" : ""}" style="background:var(${col})" data-t="${esc(`#${i + 1} ${st.name} ${JSON.stringify(st.args || {}).slice(0, 120)}`)}">${c}</span>`; }).join("");
}
function flowChain(flow) {
  return `<div class="chain">${flow.map(([who, text], i) => `${i ? '<span class="arrow">→</span>' : ""}<span class="node ${who}">${esc(text)}</span>`).join("")}</div>`;
}
function selChips(yes, band, targets, limit = 16) {
  const shown = (yes || []).slice(0, limit);
  return shown.map((p) => `<span class="chip yes${targets.includes(p) ? " target" : ""}">${esc(p)}</span>`).join("")
    + ((yes || []).length > shown.length ? `<span class="more">+${yes.length - shown.length} more</span>` : "")
    + (band || []).slice(0, 8).map((p) => `<span class="chip band" title="unsure, left folded">${esc(p)}</span>`).join("");
}

// ---------- header ----------
const totalMeasured = sum(D.evals.map((e) => e.spend_usd));
$("meta").innerHTML = [
  ["Evals", D.evals.length], ["Runs", runs.length], ["Modes tried", new Set(runs.map((r) => r.mode)).size + " of " + D.modes.length],
  ["Measured spend", usd(totalMeasured, 3)], ["Built", D.built],
].map(([k, v]) => `<span><b>${k}</b> ${esc(v)}</span>`).join("");

// ---------- overview ----------
(function () {
  const ctrl = byMode("control"), jevRuns = runs.filter((r) => r.mode !== "control");
  const met = (rs) => rs.filter((r) => r.goal_met).length;
  // A Jev setup beats the original only if, on the same test, it is correct
  // and both cheaper and faster than a correct original run.
  const wins = jevRuns.filter((r) => r.goal_met && ctrl.some((c) => c.task === r.task && c.goal_met && r.total != null && c.total != null && r.total < c.total && r.wall_s < c.wall_s));
  $("ovHeadline").textContent = wins.length
    ? `${wins.length} Jev run${wins.length === 1 ? "" : "s"} beat the original agent on the same test (correct, cheaper and faster)`
    : "No Jev setup has beaten the original agent yet (correct, cheaper and faster on the same test)";
  $("ovLede").textContent = "Each setup has mostly been run once per test, so small differences may be luck. Compare setups within one test in the grid below, not across tests.";
  $("ovTiles").innerHTML = tiles([
    [`${met(ctrl)} / ${ctrl.length}`, "original-agent runs correct"],
    [`${met(jevRuns)} / ${jevRuns.length}`, "Jev runs correct"],
    [usd(totalMeasured, 3), "spent so far, all evals"],
  ]);
  const head = D.findings.filter((f) => f.headline);
  $("ovFindings").innerHTML = head.map((f) => `<div class="finding"><span class="pill ${f.kind} tag">${esc(f.tag)}</span><p>${esc(f.finding)}</p><div class="fix"><b>Change:</b> ${esc(f.change)}</div><span class="nb-when">Eval ${esc(f.eval)} · ${esc(f.date)}</span></div>`).join("");
  const taskIds = [...new Set(runs.map((r) => r.task))];
  const modeIds = D.modes.map((m) => m.id).concat([...new Set(runs.map((r) => r.mode))].filter((m) => !MODES[m]));
  const cellRuns = (t, m) => runs.filter((r) => r.task === t && r.mode === m);
  const latest = (t, m) => cellRuns(t, m).slice(-1)[0];
  $("ovBoard").innerHTML = `<table><thead><tr><th>Test</th>${modeIds.map((m) => `<th>${esc(modeName(m))}</th>`).join("")}</tr></thead><tbody>${
    taskIds.map((t) => `<tr><td>${esc(taskPlain(t))}</td>${modeIds.map((m) => { const r = latest(t, m);
      return r ? `<td class="nb-cell">${goalPill(r.goal_met)}<span class="nb-sub">${usd(r.total)} · ${r.turns} turns${cellRuns(t, m).length > 1 ? ` · newest of ${cellRuns(t, m).length} runs` : ""}</span></td>` : `<td class="nb-empty">–</td>`; }).join("")}</tr>`).join("")}</tbody></table>`;
})();

// ---------- modes ----------
$("modeCards").innerHTML = D.modes.map((m) => {
  const rs = byMode(m.id), ok = rs.filter((r) => r.goal_met);
  // One line per test: results only compare within a test, never across tests.
  const perTest = [...new Set(rs.map((r) => r.task))].map((t) => {
    const tr = rs.filter((r) => r.task === t), good = tr.filter((r) => r.goal_met);
    return `<li>${esc(taskPlain(t))}: <b>${good.length} of ${tr.length}</b> correct · ${usd(avg(tr.map((r) => r.total)))} · ${avg(tr.map((r) => r.wall_s)).toFixed(0)} s avg</li>`;
  });
  const bestText = rs.length ? `<ul>${perTest.join("")}</ul>` : "Not tried yet.";
  return `<div class="nb-card"><div class="arm-top"><div><div class="arm-name">${esc(m.name)}</div></div><span class="pill neutral">${rs.length} run${rs.length === 1 ? "" : "s"}</span></div>
    <p>${esc(m.what)}</p>${flowChain(m.flow)}<span class="flags">${m.flags.length ? esc(m.flags.join(" ")) : "no flags"}</span><div class="nb-best">${bestText}</div></div>`;
}).join("");

// ---------- tests ----------
$("testCards").innerHTML = D.tasks.map((t) => {
  const rs = runs.filter((r) => r.task === t.id);
  const style = t.prompt_style === "symptoms" ? `<span class="pill good">describes symptoms</span>` : `<span class="pill warn">names the code to fix</span>`;
  return `<div class="nb-card"><div class="arm-top"><div><div class="arm-name">${esc(t.name || t.id)}</div><div class="arm-sub">${esc(t.id)} · ${t.lines} lines · ${t.bindings ?? "?"} definitions</div></div>${style}</div>
    <p>${esc(t.what || "")}</p><p><b>What is broken:</b> ${esc(t.broken || "")}</p>
    <div class="chips">${(t.nav_targets || []).map((p) => `<span class="chip target">${esc(p)}</span>`).join("")}</div>
    <details><summary>The request the agent gets</summary><p style="white-space:pre-wrap;font-size:13px;margin-top:6px">${esc(t.prompt)}</p></details>
    <div class="nb-best">Correct result: <code>${esc(t.goal)}</code> · turn cap ${t.max_turns}<br>${rs.length ? `${rs.filter((r) => r.goal_met).length} of ${rs.length} runs correct (${[...new Set(rs.map((r) => modeName(r.mode)))].join(", ")})` : "No runs yet."}</div></div>`;
}).join("");

// ---------- all runs ----------
const filters = { mode: null, task: null };
function filterChips(el, key, values, label) {
  el.innerHTML = [`<button class="chip" data-v="">All</button>`, ...values.map((v) => `<button class="chip" data-v="${esc(v)}">${esc(label(v))}</button>`)].join("");
  el.querySelectorAll("button").forEach((b) => b.addEventListener("click", () => { filters[key] = b.dataset.v || null; renderRuns(); }));
}
function renderRuns() {
  ["fMode", "fTask"].forEach((id, i) => $(id).querySelectorAll("button").forEach((b) => b.setAttribute("aria-pressed", String((b.dataset.v || null) === filters[i ? "task" : "mode"]))));
  const rs = runs.filter((r) => (!filters.mode || r.mode === filters.mode) && (!filters.task || r.task === filters.task));
  $("runTable").innerHTML = `<table><thead><tr><th>Eval</th><th>Test</th><th>Setup</th><th>Correct?</th><th>Turns</th><th>Total $</th><th>Agent $</th><th>Jev $</th><th>Time</th><th>View requests</th><th>Jev opens that were needed</th><th>Needed code Jev opened</th></tr></thead><tbody>${
    rs.map((r) => `<tr data-i="${r.i}"><td class="n">${esc(r.eval)}</td><td class="nb-test">${esc(taskPlain(r.task))}</td><td>${esc(modeName(r.mode))}</td><td>${goalPill(r.goal_met)}</td>
      <td class="n">${r.turns}</td><td class="n">${usd(r.total)}</td><td class="n">${usd(r.cost_main)}</td><td class="n">${r.jev ? usd(r.jev) : "–"}</td><td class="n">${r.wall_s.toFixed(0)}s</td><td class="n">${r.nav_calls}</td>
      <td class="n">${r.precision == null ? "–" : pct(r.precision)}</td><td class="n">${r.recall == null ? "–" : pct(r.recall)}</td></tr>`).join("") || `<tr><td colspan="12" class="nb-empty">No runs match.</td></tr>`}</tbody></table>`;
  $("runTable").querySelectorAll("tbody tr[data-i]").forEach((tr) => tr.addEventListener("click", () => { showRun(Number(tr.dataset.i)); showTab("details"); history.replaceState(null, "", "#details"); }));
}
filterChips($("fMode"), "mode", [...new Set(runs.map((r) => r.mode))], modeName);
filterChips($("fTask"), "task", [...new Set(runs.map((r) => r.task))], (t) => (TASKS[t] && TASKS[t].name) || t);
renderRuns();

// ---------- run details ----------
function argText(st) {
  const a = st.args || {};
  if (a.intent && !a.path) return "intent: " + a.intent;
  if (a.code) return (a.path ? "path: " + a.path + "\n" : "") + a.code;
  if (a.sketch || a.intent) return Object.entries(a).map(([k, v]) => `${k}: ${Array.isArray(v) ? v.join(", ") : v}`).join("\n");
  if (a.paths) return "paths: " + a.paths.join(", ");
  return JSON.stringify(a);
}
function showRun(i) {
  const r = runs[i];
  $("dPick").value = String(i);
  $("dTitle").textContent = runLabel(r);
  $("dSub").innerHTML = `${esc(taskPlain(r.task))} · ${esc((MODES[r.mode] || {}).name || r.mode)} · source <code>${esc(r.source)}</code>`;
  $("dTiles").innerHTML = tiles([
    [r.goal_met ? "✓ correct output" : "✗ wrong output", "program's final output vs. the known right answer" + (r.final_value ? ` (got ${esc(String(r.final_value).slice(0, 40))})` : "")],
    [String(r.turns), `agent turns (cap ${(TASKS[r.task] || {}).max_turns || 24})`],
    [usd(r.total), `total · agent ${usd(r.cost_main)} · Jev ${usd(r.jev)}`],
    [r.wall_s.toFixed(0) + " s", "wall time"],
    [String(r.nav_calls), "navigation calls"],
  ]);
  $("dTimeline").innerHTML = r.steps.length
    ? `<div class="tl-row"><div class="tl-label">${r.steps.length} calls</div><div class="cells">${timelineCells(r.steps)}</div></div>`
    : `<p class="nb-empty">No per-call log was captured for this run.</p>`;
  $("dTimeline").querySelectorAll(".cell").forEach((m) => hoverable(m, m.dataset.t));
  $("dSteps").innerHTML = r.steps.map((st, k) => { const [, col] = cat(st.name);
    return `<div class="step${st.ok === false ? " fail" : ""}"><span class="i">${k + 1}</span><span class="nm" style="color:var(${col})">${esc(st.name)}${st.ok === false ? " ✗" : ""}</span><code>${hl(argText(st).slice(0, 700))}${st.ok === false ? "\n→ " + esc(st.content) : ""}</code></div>`; }).join("") || `<p class="nb-empty">None.</p>`;
  $("dSelWrap").hidden = !r.selections.length;
  $("dSels").innerHTML = r.selections.map((s, k) => {
    const intent = (s.intent || "").replace(/\s+/g, " ");
    return `<div class="sel"><span class="sel-idx">#${k + 1}</span><div><div class="sel-intent">${esc(intent.length > 180 ? intent.slice(0, 180) + "…" : intent)}</div><div class="chips">${selChips(s.yes, s.band, r.targets)}</div></div><span class="sel-lat">${(s.yes || []).length} open · ${Number(s.latency_ms || 0).toFixed(0)} ms${s.failed ? " · failed" : ""}</span></div>`;
  }).join("");
  $("dEditWrap").hidden = !r.edits.length;
  $("dEdits").innerHTML = r.edits.map((e, k) => `<div class="sel"><span class="sel-idx">#${k + 1}</span><div><div class="sel-intent"><code>${esc(e.path)}</code> · ${esc(e.intent || "")}</div><div class="chips"><span class="chip target">${e.filled} filled</span>${(e.escalated || []).map((h) => `<span class="chip band" title="${esc((h.candidates || []).slice(0, 12).join(" | "))}">handed back: ${esc(h.expected_type)}</span>`).join("")}${e.error ? `<span class="chip">${esc(e.error)}</span>` : ""}</div></div><span class="sel-lat">${e.rounds} round${e.rounds === 1 ? "" : "s"} · ${Number(e.latency_ms || 0).toFixed(0)} ms</span></div>`).join("");
  const diffs = r.steps.filter((s) => s.ok && (s.old || s.new || (s.args && s.args.code)));
  $("dDiffs").innerHTML = diffs.map((s) => `<div class="diff"><div class="code"><div class="code-h"><span>${esc((s.args && s.args.path) || s.name)} · before</span><span class="pill bad">old</span></div><pre>${hl(s.old || "(not captured)")}</pre></div><div class="code"><div class="code-h"><span>after</span><span class="pill good">new</span></div><pre>${hl(s.new || (s.args && s.args.code) || "")}</pre></div></div>`).join("") || `<p class="nb-empty">No code changes captured.</p>`;
}
$("dPick").innerHTML = runs.map((r) => `<option value="${r.i}">${esc(runLabel(r))}</option>`).join("");
$("dPick").addEventListener("change", (e) => showRun(Number(e.target.value)));
if (runs.length) showRun(runs.length - 1);

// ---------- findings ----------
$("findingLog").innerHTML = [...D.findings].reverse().map((f) => `<div class="nb-entry finding"><span class="nb-when">${esc(f.date)} · ${f.eval === "-" ? "" : "Eval " + esc(f.eval) + " · "}${esc(f.round)}</span><span class="pill ${f.kind} tag">${esc(f.tag)}</span><p>${esc(f.finding)}</p><div class="fix"><b>Change:</b> ${esc(f.change)}</div></div>`).join("");

// ---------- spend ----------
(function () {
  const logged = sum(runs.map((r) => r.total));
  const taskIds = [...new Set(runs.map((r) => r.task))];
  $("spTiles").innerHTML = tiles([
    [usd(totalMeasured, 3), "measured spend, all evals"],
    [usd(logged, 3), "logged spend, all runs"],
    [usd(avg(runs.map((r) => r.total))), "average per run"],
    [usd(sum(runs.map((r) => r.jev))), "of which Jev"],
    ...D.evals.map((e) => [usd(e.spend_usd, 3), `Eval ${esc(e.id)} (${runs.filter((r) => r.eval === e.id).length} runs), measured`]),
  ]);
  const ordered = [...runs].sort((a, b) => a.task.localeCompare(b.task) || a.eval.localeCompare(b.eval));
  hbars($("spRuns"), ordered, (r) => (TASKS[r.task] || {}).name || r.task, (r) => `Eval ${r.eval} · ${modeName(r.mode)}${r.goal_met ? "" : " ✗"}`, costParts, (v) => usd(v));
  const perTask = taskIds.map((t) => { const rs = runs.filter((r) => r.task === t); return { task: t, n: rs.length, cost_main: avg(rs.map((r) => r.cost_main)), cost_nav: avg(rs.map((r) => r.cost_nav)), cost_edit: avg(rs.map((r) => r.cost_edit)) }; });
  hbars($("spTasks"), perTask, () => "", (p) => `${(TASKS[p.task] || {}).name || p.task} (n=${p.n})`, costParts, (v) => usd(v));
  $("spTable").innerHTML = `<table><thead><tr><th>Eval</th><th>What</th><th>Runs</th><th>Measured</th><th>Logged</th><th>Per run</th></tr></thead><tbody>${
    D.evals.map((e) => { const rs = runs.filter((r) => r.eval === e.id); return `<tr><td class="n">${esc(e.id)}</td><td>${esc(e.title)}</td><td class="n">${rs.length}</td><td class="n">${usd(e.spend_usd, 4)}</td><td class="n">${usd(sum(rs.map((r) => r.total)), 4)}</td><td class="n">${usd(avg(rs.map((r) => r.total)))}</td></tr>`; }).join("")}</tbody></table>`;
})();

showTab(location.hash.slice(1) || "overview");
