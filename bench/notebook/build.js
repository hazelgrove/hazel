#!/usr/bin/env node
// Jev Lab Notebook: one self-contained HTML page covering every eval run,
// every mode (control + each Jev variation) and every test.
//
//   node bench/notebook/build.js [OUT.html]      # default: a new temp dir
//
// Inputs are read-only: bench/notebook/{evals,modes,tasks-meta,findings}.json
// (hand-maintained), each eval's data file, bench/results/jev-eval-*/ (rows +
// transcripts) and bench/tasks/*. The page reuses the Eval 001/002 report
// styles verbatim (ref-*.html here) plus notebook.css, and renders in the
// browser from one embedded JSON blob (client.js).
//
// Output never goes under docs/ unless --allow-docs is passed, and the build
// fails, writing nothing, if the page could contain an API key.

const fs = require("fs");
const os = require("os");
const path = require("path");

const ROOT = path.resolve(__dirname, "..", "..");
const HERE = __dirname;
const readJson = (p) => JSON.parse(fs.readFileSync(p, "utf8"));
const readText = (p) => fs.readFileSync(p, "utf8");
const exists = (p) => fs.existsSync(p);

// ---------- modes ----------

// A run's mode is decided by its flags, so a run recorded under any arm label
// lands in the right mode. Combinations without their own card get a name
// built from their parts.
const modeOfFlags = (f) => {
  const p = !!f.jev_prepass, v = !!f.jev_view_tool, b = !!f.jev_edit_builds, e = !!f.jev_edit_tool || b;
  if (!p && !v && !e) return "control";
  if (p && v && b) return "jev";
  if (p && !v && !e) return "prepass";
  if (v && !p && !e) return "view";
  if (e && !b && !p && !v) return "edit-sketch";
  if (b && !p && !v) return "edit-build";
  return [p && "prepass", v && "view", e && (b ? "build" : "edit")].filter(Boolean).join("+");
};

// Rows trimmed for an eval data file carry no flags; their arm label is all
// that is left, and these labels are the study's own.
const MODE_OF_ARM = { control: "control", prepass: "prepass", view: "view", edit: "edit-sketch", "edit+build": "edit-build", jev: "jev" };

// ---------- runs: one normalized shape from every source ----------

const navCallsOf = (tools) => (tools.expand || 0) + (tools.collapse || 0) + (tools.modify_view || 0);

// Steps from an AgentRun transcript: every tool call, flattened, in order.
const stepsOfTranscript = (tx) =>
  (tx.events || []).flatMap((ev) =>
    (ev.tool_calls || []).map((c) => ({
      turn: ev.turn,
      name: c.name,
      args: c.args,
      ok: c.result.success,
      content: c.result.content,
      old: c.result.diff ? c.result.diff.old : null,
      new: c.result.diff ? c.result.diff.new : null,
    })),
  );

const runOfRow = ({ evalId, source, row, tx, taskOf }) => {
  const m = row.main;
  const mode = row.flags ? modeOfFlags(row.flags) : MODE_OF_ARM[row.arm] || row.arm;
  const task = row.task;
  return {
    eval: evalId,
    source,
    task,
    arm: row.arm,
    mode,
    goal_met: row.outcome.goal_met,
    said_done: row.outcome.said_done,
    turns: m.turns,
    wall_s: row.wall_ms / 1000,
    cost_main: m.cost_usd,
    cost_nav: row.jev ? row.jev.cost_usd : 0,
    cost_edit: row.jev_edit ? row.jev_edit.cost_usd : 0,
    tools: m.tool_calls,
    nav_calls: navCallsOf(m.tool_calls),
    tokens: { prompt: m.prompt_tokens, cached: m.cached_tokens, completion: m.completion_tokens },
    precision: row.selection ? row.selection.precision : null,
    recall: row.selection ? row.selection.recall : null,
    selections: (tx && (tx.jev_selections || tx.selections)) || (row.jev && row.jev.selections) || [],
    edits: (tx && tx.jev_edits) || (row.jev_edit && row.jev_edit.edits) || [],
    steps: tx ? (tx.events ? stepsOfTranscript(tx) : tx.steps || []) : [],
    api_failures: row.outcome.api_failures || [],
    final_value: tx ? tx.final_value : null,
    start_program: tx && tx.program ? tx.program : taskOf(task).program_text,
    final_program: tx ? tx.final_program : null,
    targets: taskOf(task).nav_targets || [],
  };
};

// Eval 001 predates the metrics row; its data file has its own shape.
const runsOfEval001 = (ev, d, taskOf) => {
  const base = (arm, a) => ({
    eval: ev.id, source: ev.data_file, task: "fix-middle", arm, mode: arm, goal_met: a.goal_met, said_done: !!a.said_done,
    turns: a.turns, wall_s: a.wall_s, cost_main: a.main_cost, tools: a.tools, nav_calls: navCallsOf(a.tools),
    tokens: { prompt: a.prompt, cached: a.cached, completion: a.completion },
    api_failures: [], start_program: d.start, targets: d.targets, final_program: null,
  });
  return [
    { ...base("control", d.control), cost_nav: 0, cost_edit: 0, precision: null, recall: null, selections: [], edits: [],
      final_value: d.control.final_value,
      steps: d.control.actions.map(([name, p, code]) => ({ turn: null, name, args: { path: p, code }, ok: true, content: "", old: null, new: null })) },
    { ...base("jev", d.jev), cost_nav: d.jev.nav.cost_usd, cost_edit: d.jev.edit.cost_usd,
      precision: d.jev.selection.precision, recall: d.jev.selection.recall,
      selections: d.jev.nav.selections, edits: d.jev.edit.edits, steps: [], final_value: null },
  ];
};

const readResultsDir = (dir) => {
  const rowsFile = path.join(dir, "rows.jsonl");
  if (!exists(rowsFile)) return [];
  const rows = readText(rowsFile).split("\n").filter(Boolean).map(JSON.parse);
  const txFiles = fs.readdirSync(dir).filter((f) => f.endsWith(".transcript.json"));
  const used = new Set();
  return rows.map((row) => {
    // Rows written since C6 name their transcript; older ones are matched by
    // task, arm and planner turns, taking each file at most once.
    let file = row.transcript;
    if (!file) {
      file = txFiles.find((f) => !used.has(f) && f.startsWith(`${row.task}.${row.arm}.`) &&
        (readJson(path.join(dir, f)).events || []).filter((e) => e.kind === "agent").length === row.main.turns);
    }
    if (file) used.add(file);
    return { row, tx: file && exists(path.join(dir, file)) ? readJson(path.join(dir, file)) : null };
  });
};

// The same run can sit in an eval data file and in a results folder; the
// folder copy is richer (full row, transcript), so it wins.
const sameRun = (a, b) => a.task === b.task && a.arm === b.arm && Math.abs(a.wall_ms - b.wall_ms) < 1;

const loadRuns = (evals, taskOf) => {
  const resultsRoot = path.join(ROOT, "bench", "results");
  const allDirs = exists(resultsRoot) ? fs.readdirSync(resultsRoot).filter((d) => d.startsWith("jev-eval-")) : [];
  const claimed = new Set(evals.flatMap((e) => e.results_dirs || []));
  const unfiled = allDirs.filter((d) => !claimed.has(d)).map((d) => ({
    id: "new " + d.slice(-6, -2), title: `Not yet written up: bench/results/${d}`, date: d.slice(9, 17).replace(/(\d{4})(\d\d)(\d\d)/, "$1-$2-$3"),
    round: "unknown", spend_usd: null, results_dirs: [d], format: "results",
  }));
  const runs = [];
  for (const ev of [...evals, ...unfiled]) {
    const fromDirs = (ev.results_dirs || []).flatMap((d) =>
      readResultsDir(path.join(resultsRoot, d)).map((x) => ({ ...x, source: `bench/results/${d}` })));
    const data = ev.data_file && exists(path.join(ROOT, ev.data_file)) ? readJson(path.join(ROOT, ev.data_file)) : null;
    if (ev.format === "eval001" && data) runs.push(...runsOfEval001(ev, data, taskOf));
    if (ev.format === "eval002" && data) {
      for (const r of data.eval002) {
        const row = { ...r.row, task: r.task, arm: r.arm };
        if (!fromDirs.some((x) => sameRun(x.row, row))) {
          runs.push(runOfRow({ evalId: ev.id, source: ev.data_file, row, tx: r.tx, taskOf }));
        }
      }
    }
    for (const x of fromDirs) runs.push(runOfRow({ evalId: ev.id, source: x.source, row: x.row, tx: x.tx, taskOf }));
  }
  // A results folder with no rows yet is an eval still running; leave it out
  // until it has something to show.
  const withRuns = unfiled.filter((e) => runs.some((r) => r.eval === e.id));
  return { runs, evals: [...evals, ...withRuns] };
};

// ---------- tasks ----------

const loadTasks = () => {
  const meta = readJson(path.join(HERE, "tasks-meta.json"));
  const dir = path.join(ROOT, "bench", "tasks");
  const tasks = {};
  for (const f of fs.readdirSync(dir).filter((f) => f.endsWith(".json"))) {
    const t = readJson(path.join(dir, f));
    const hz = path.join(ROOT, t.program);
    const text = exists(hz) ? readText(hz) : "";
    tasks[t.name] = {
      id: t.name, goal: t.goal, prompt: t.prompt, max_turns: t.max_turns || 24, nav_targets: t.nav_targets || [],
      lines: text.split("\n").filter((l) => l.trim() !== "").length, program_text: text,
      ...(meta[t.name] || {}), has_meta: !!meta[t.name],
    };
  }
  return tasks;
};

// ---------- page ----------

// The references' <style> blocks are used as they are, so the notebook keeps
// their look, tokens and dark mode without a second copy to drift.
const styleBlocks = (html) => [...html.matchAll(/<style>([\s\S]*?)<\/style>/g)].map((m) => m[1]).join("\n");

const page = (data) => {
  const ref1 = readText(path.join(HERE, "ref-eval-001.template.html"));
  const ref2 = readText(path.join(HERE, "ref-eval-002.body.html"));
  const fonts = (ref1.match(/<link rel="preconnect"[^>]*>\s*<link rel="stylesheet"[^>]*>/) || [""])[0];
  // "</" inside the JSON would end the script element early.
  const json = JSON.stringify(data).replace(/<\//g, "<\\/");
  return [
    "<title>Jev Lab Notebook</title>",
    fonts,
    `<style>\n${styleBlocks(ref1)}\n${styleBlocks(ref2)}\n${readText(path.join(HERE, "notebook.css"))}\n</style>`,
    readText(path.join(HERE, "body.html")),
    `<script>\nconst D = ${json};\n${readText(path.join(HERE, "client.js"))}\n</script>`,
  ].join("\n");
};

// Refuses to write anything that looks like an OpenRouter key, or the key in
// this process's environment. Nothing from settings is read, but the inputs
// are run logs, so check the output rather than trust them.
const assertNoKey = (html) => {
  const key = process.env.OPENROUTER_API_KEY;
  if (html.includes("sk-or-") || (key && key.length > 8 && html.includes(key))) {
    throw new Error("output contains an API key pattern (sk-or-); nothing was written");
  }
};

const outPath = (args) => {
  const explicit = args.find((a) => !a.startsWith("--"));
  const out = explicit
    ? path.resolve(explicit)
    : path.join(fs.mkdtempSync(path.join(os.tmpdir(), "jev-notebook-")), "jev-lab-notebook.html");
  const docs = path.join(ROOT, "docs") + path.sep;
  if (out.startsWith(docs) && !args.includes("--allow-docs")) {
    throw new Error(`refusing to write under docs/ (${out}); pass --allow-docs if that is intended`);
  }
  return out;
};

const main = () => {
  const args = process.argv.slice(2);
  const out = outPath(args);
  const tasks = loadTasks();
  const taskOf = (id) => tasks[id] || { nav_targets: [], program_text: "" };
  const { runs, evals } = loadRuns(readJson(path.join(HERE, "evals.json")), taskOf);
  const data = {
    built: new Date().toISOString().slice(0, 16).replace("T", " "),
    modes: readJson(path.join(HERE, "modes.json")),
    findings: readJson(path.join(HERE, "findings.json")),
    evals,
    // Only tests with a write-up or at least one run belong in the notebook.
    tasks: Object.values(tasks)
      .filter((t) => t.has_meta || runs.some((r) => r.task === t.id))
      .map(({ program_text, ...t }) => t),
    runs,
  };
  const html = page(data);
  assertNoKey(html);
  fs.writeFileSync(out, html);
  console.log(`wrote ${out} (${runs.length} runs, ${evals.length} evals, ${(html.length / 1024).toFixed(0)} KB)`);
};

try {
  main();
} catch (e) {
  console.error("error: " + e.message);
  process.exit(1);
}
