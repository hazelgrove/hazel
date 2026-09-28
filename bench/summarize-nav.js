#!/usr/bin/env node
// Summarize a Jev navigation study: medians per task x arm, and ratios against
// that task's control arm.
//
//   node bench/summarize-nav.js [bench/results/nav-study.jsonl]
//   node bench/summarize-nav.js --verdict FILE   # one line per run; exit 1 on errors
//
// Input rows are written by `hazel agent --metrics-out` (AgentRun.Metrics.row).
// Medians, not means: agent runs are heavy-tailed (one looping run can cost 10x
// the rest), and with ~5 reps a mean is mostly that one run.

const fs = require("fs");

const verdictMode = process.argv.includes("--verdict");
const path = process.argv.filter((a) => a !== "--verdict")[2] || "bench/results/nav-study.jsonl";

const readRows = (file) =>
  fs
    .readFileSync(file, "utf8")
    .split("\n")
    .filter((line) => line.trim() !== "")
    .map((line) => JSON.parse(line));

const median = (xs) => {
  const ys = xs.filter((x) => typeof x === "number").sort((a, b) => a - b);
  if (ys.length === 0) return null;
  const mid = Math.floor(ys.length / 2);
  return ys.length % 2 ? ys[mid] : (ys[mid - 1] + ys[mid]) / 2;
};

const rate = (bools) => {
  const known = bools.filter((b) => typeof b === "boolean");
  return known.length ? known.filter(Boolean).length / known.length : null;
};

// Batch size only distinguishes arms where Jev navigates; control and the
// edit-only arm never send a navigation request, so they group whatever
// --jev-batch they carried.
const armLabel = (row) =>
  row.flags.jev_prepass || row.flags.jev_view_tool
    ? `${row.arm}@${row.flags.jev_batch_max_tokens}`
    : row.arm || "unlabelled";

// Rows from before the edit arm existed have no jev_edit section.
const jevEdit = (row) => row.jev_edit || {};

// Everything the run paid for: main model plus both Jev roles. Null when the
// main model reported no cost (e.g. --stub), rather than a misleading 0.
const totalCost = (row) =>
  row.main.cost_usd == null ? null : row.main.cost_usd + row.jev.cost_usd + (jevEdit(row).cost_usd || 0);

// Per jev_edit call rather than per run: build mode trades fewer planner
// turns for more Jev rounds and holes on each call, which a per-run total
// would blur with how many calls the planner made.
const perEdit = (row, field) => {
  const e = jevEdit(row);
  return e.count ? e[field] / e.count : null;
};

const refusedCalls = (row) => (row.main.tool_calls.refused_nav || 0) + (row.main.tool_calls.refused_edit || 0);

const navCalls = (row) => {
  const t = row.main.tool_calls;
  return t.expand + t.collapse + t.modify_view;
};

// One entry per reported column: how to read it off a row, and whether a
// ratio against control means anything (a rate or a Jev-only number does not).
const COLUMNS = [
  ["n", (rows) => rows.length, false],
  ["nav_calls", (rows) => median(rows.map(navCalls)), true],
  ["turns", (rows) => median(rows.map((r) => r.main.turns)), true],
  ["main_cost", (rows) => median(rows.map((r) => r.main.cost_usd)), true],
  ["total_cost", (rows) => median(rows.map(totalCost)), true],
  ["wall_s", (rows) => median(rows.map((r) => r.wall_ms / 1000)), true],
  ["refused", (rows) => median(rows.map(refusedCalls)), false],
  ["jev_req", (rows) => median(rows.map((r) => r.jev.requests)), false],
  ["jev_cost", (rows) => median(rows.map((r) => r.jev.cost_usd)), false],
  ["edit_fill", (rows) => median(rows.map((r) => jevEdit(r).fill_rate)), false],
  ["edit_esc", (rows) => median(rows.map((r) => jevEdit(r).escalated)), false],
  ["holes/edit", (rows) => median(rows.map((r) => perEdit(r, "holes_seen"))), false],
  ["rounds/edit", (rows) => median(rows.map((r) => perEdit(r, "rounds"))), false],
  ["edit_lat_s", (rows) => median(rows.map((r) => (jevEdit(r).latency_ms == null ? null : jevEdit(r).latency_ms / 1000))), false],
  ["goal_met", (rows) => rate(rows.map((r) => r.outcome.goal_met)), false],
  ["recall", (rows) => median(rows.map((r) => r.selection && r.selection.recall)), false],
  ["precision", (rows) => median(rows.map((r) => r.selection && r.selection.precision)), false],
];

const summarize = (rows) => {
  const groups = new Map();
  for (const row of rows) {
    const key = `${row.task}\t${armLabel(row)}`;
    if (!groups.has(key)) groups.set(key, []);
    groups.get(key).push(row);
  }
  const stats = new Map(
    [...groups].map(([key, rs]) => [key, Object.fromEntries(COLUMNS.map(([name, f]) => [name, f(rs)]))]),
  );
  return [...stats]
    .sort(([a], [b]) => a.localeCompare(b))
    .map(([key, s]) => {
      const [task, arm] = key.split("\t");
      const control = stats.get(`${task}\tcontrol`);
      const ratios = Object.fromEntries(
        COLUMNS.filter(([, , hasRatio]) => hasRatio).map(([name]) => [
          name,
          control && control[name] ? s[name] / control[name] : null,
        ]),
      );
      return { task, arm, ...s, ratios };
    });
};

const fmt = (x) => (x == null ? "-" : typeof x === "string" ? x : Number.isInteger(x) ? String(x) : x.toFixed(x < 1 ? 4 : 2));

const printTable = (summary) => {
  const header = ["task", "arm", ...COLUMNS.map(([name]) => name), "x_nav", "x_turns", "x_cost", "x_total", "x_wall"];
  const lines = summary.map((s) => [
    s.task,
    s.arm,
    ...COLUMNS.map(([name]) => fmt(s[name])),
    fmt(s.ratios.nav_calls),
    fmt(s.ratios.turns),
    fmt(s.ratios.main_cost),
    fmt(s.ratios.total_cost),
    fmt(s.ratios.wall_s),
  ]);
  const widths = header.map((h, i) => Math.max(h.length, ...lines.map((l) => l[i].length)));
  const pad = (cells) => cells.map((c, i) => c.padEnd(widths[i])).join("  ");
  console.log(pad(header));
  lines.forEach((l) => console.log(pad(l)));
  console.log("\nx_* = median / control median for the same task (below 1 = fewer/cheaper than control).");
};

// The first thing that went wrong in a run, if anything did: an API failure
// the reducer recorded, else a Jev edit or view selection that failed.
const firstError = (row) => {
  const api = (row.outcome.api_failures || [])[0];
  const edit = (jevEdit(row).edits || []).find((e) => e.failed);
  const nav = (row.jev.selections || []).find((s) => s.failed);
  const text = api || (edit && `jev_edit: ${edit.error}`) || (nav && `jev nav failed (${nav.intent})`);
  return text ? String(text).split("\n")[0] : null;
};

const printVerdicts = (rows) => {
  const cells = rows.map((r) => ({
    task: r.task,
    arm: r.arm,
    goal_met: fmt(r.outcome.goal_met == null ? null : String(r.outcome.goal_met)),
    turns: fmt(r.main.turns),
    main_cost: fmt(r.main.cost_usd),
    jev_req: fmt(r.jev.requests),
    edit_fill: fmt(jevEdit(r).fill_rate),
    refused: fmt(refusedCalls(r)),
    error: firstError(r) || "-",
  }));
  const header = Object.keys(cells[0] || { task: 0 });
  const widths = header.map((h) => Math.max(h.length, ...cells.map((c) => String(c[h]).length)));
  const line = (vals) => vals.map((v, i) => (i === vals.length - 1 ? String(v) : String(v).padEnd(widths[i]))).join("  ");
  console.log(line(header));
  cells.forEach((c) => console.log(line(header.map((h) => c[h]))));
  return cells.filter((c) => c.error !== "-").length;
};

const rows = readRows(path);
if (verdictMode) {
  const errored = printVerdicts(rows);
  if (errored > 0) {
    console.log(`\n${errored} run(s) reported errors.`);
    process.exit(1);
  }
} else {
  printTable(summarize(rows));
}
