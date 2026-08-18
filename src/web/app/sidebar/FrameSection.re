open Virtual_dom.Vdom;

/* The "Frame Timing" debug sidebar section: the per-keystroke timeline. One row
   per recent frame — the edit action's perform (update phase) then the
   calculate-phase stages (statics / syntax rebuild / cursor info / color maps)
   and the grand total — plus a rolling max of the total. A stage shows `—` when
   it didn't run that frame (e.g. statics deferred by the debounce). Read from
   PerfMetrics, populated across CodeEditable / CodeWithStatics / Page while this
   panel is open. Implements DebugSection.S. */

let title = "Frame Timing";

/* All cells share one scale (the peak total across frames), so a stage as red
   as the total means it dominates that frame, and darker rows are slower
   frames overall. */
let columns =
    (~max: Core.Time_ns.Span.t): list(PerfFormat.column(PerfMetrics.frame)) => [
  {
    label: "action",
    tooltip: "The edit action that triggered this frame.",
    cell: f => PerfFormat.label_cell(PerfFormat.fmt_opt(fst, f.perform)),
  },
  {
    label: "perform",
    tooltip: "Update phase: applying the edit action to the zipper (Perform.go).",
    cell: f => PerfFormat.heat_cell(~max, Option.map(snd, f.perform)),
  },
  {
    label: "statics",
    tooltip: "Calculate phase: statics + elaboration recompute (— when deferred/cached).",
    cell: f => PerfFormat.heat_cell(~max, f.statics),
  },
  {
    label: "syntax",
    tooltip: "Calculate phase: syntax-cache rebuild (MakeTerm + Measured).",
    cell: f => PerfFormat.heat_cell(~max, f.syntax),
  },
  {
    label: "cursor",
    tooltip: "Calculate phase: computing cursor info for the sidebar and decorations.",
    cell: f => PerfFormat.heat_cell(~max, f.cursor_info),
  },
  {
    label: "colors",
    tooltip: "Calculate phase: building the ExplainThis color-highlight map.",
    cell: f => PerfFormat.heat_cell(~max, f.color_map),
  },
  {
    label: "total",
    tooltip: "Whole keystroke: perform (update phase) + the entire calculate phase.",
    cell: f => PerfFormat.heat_cell(~max, ~total=true, f.total),
  },
];

let view = (~globals as _: Globals.t): list(Node.t) =>
  switch (PerfMetrics.history^) {
  | [] => [
      PerfFormat.empty("No frames recorded yet — type in the editor."),
    ]
  | frames =>
    /* One scale for the whole table: the peak total across frames. */
    let max =
      PerfFormat.max_span(
        List.map((f: PerfMetrics.frame) => f.total, frames),
      );
    [
      PerfFormat.note(
        "max total: " ++ PerfFormat.fmt_span(max) ++ " · redder = slower",
      ),
      PerfFormat.table(
        ~columns=columns(~max),
        List.map(f => PerfFormat.Row(f), frames),
      ),
    ];
  };
