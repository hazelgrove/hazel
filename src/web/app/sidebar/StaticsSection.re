open Virtual_dom.Vdom;

/* The "Statics" debug sidebar section: a per-frame history of statics +
   elaboration — how long each recompute took (`—` when the debounce deferred
   it or the cache was reused, with the reason in the `mode` column) and the
   resulting info-map size / error / warning counts, plus the triggering edit
   action. Read from PerfMetrics.history, populated in CodeWithStatics.calculate
   while this panel is open. Implements DebugSection.S. */

let title = "Statics";

/* The panel names each outcome by its constructor, lowercased — no hand-written
   map to fall out of step with the variant. */
let mode_label = (m: PerfMetrics.statics_mode): string =>
  String.lowercase_ascii(PerfMetrics.show_statics_mode(m));

let columns: list(PerfFormat.column(PerfMetrics.frame)) = [
  PerfFormat.action_column((f: PerfMetrics.frame) =>
    PerfFormat.fmt_opt(fst, f.perform)
  ),
  {
    label: "time",
    tooltip: "Time to recompute statics + elaboration (Statics.mk). — when the recompute was skipped this frame.",
    cell: f => PerfFormat.total_cell(f.statics),
  },
  {
    label: "entries",
    tooltip: "Number of entries in the info map (one per expression id).",
    cell: f => PerfFormat.int_cell(f.info_map_entries),
  },
  {
    label: "err",
    tooltip: "Number of static error ids.",
    cell: f => PerfFormat.int_cell(f.errors),
  },
  {
    label: "warn",
    tooltip: "Number of static warning ids.",
    cell: f => PerfFormat.int_cell(f.warnings),
  },
  {
    label: "mode",
    tooltip: "Whether statics recomputed this frame: recomputed, forced, deferred (debounced), or cached (reused).",
    cell: f =>
      PerfFormat.text_cell(PerfFormat.fmt_opt(mode_label, f.statics_mode)),
  },
];

let view = (~globals as _: Globals.t): list(Node.t) =>
  switch (PerfMetrics.history^) {
  | [] => [
      PerfFormat.empty("No statics recorded yet — type in the editor."),
    ]
  | frames => [
      PerfFormat.table(~columns, List.map(f => PerfFormat.Row(f), frames)),
    ]
  };
