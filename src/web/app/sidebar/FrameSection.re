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
let row = (~max: Core.Time_ns.Span.t, f: PerfMetrics.frame): Node.t =>
  Node.tr([
    PerfFormat.action_cell(PerfFormat.action_of(f.perform)),
    PerfFormat.heat_cell(~max, f.perform |> Option.map(snd)),
    PerfFormat.heat_cell(~max, f.statics),
    PerfFormat.heat_cell(~max, f.syntax),
    PerfFormat.heat_cell(~max, f.cursor_info),
    PerfFormat.heat_cell(~max, f.color_map),
    PerfFormat.heat_cell(~max, ~cls=["perf-total"], f.total),
  ]);

let view = (~globals as _: Globals.t): list(Node.t) =>
  switch (PerfMetrics.frames^) {
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
        "max total: " ++ PerfFormat.span(max) ++ " · redder = slower",
      ),
      PerfFormat.table([
        PerfFormat.head_row([
          ("action", "The edit action that triggered this frame."),
          (
            "perform",
            "Update phase: applying the edit action to the zipper (Perform.go).",
          ),
          (
            "statics",
            "Calculate phase: statics + elaboration recompute (— when deferred/cached).",
          ),
          (
            "syntax",
            "Calculate phase: syntax-cache rebuild (MakeTerm + Measured).",
          ),
          (
            "cursor",
            "Calculate phase: computing cursor info for the sidebar and decorations.",
          ),
          (
            "colors",
            "Calculate phase: building the ExplainThis color-highlight map.",
          ),
          (
            "total",
            "Whole keystroke: perform (update phase) + the entire calculate phase.",
          ),
        ]),
        ...List.map(row(~max), frames),
      ]),
    ];
  };
