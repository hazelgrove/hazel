open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;

/* The "Statics" debug sidebar section: a per-frame history of statics +
   elaboration — how long each recompute took (`—` when the debounce deferred
   it or the cache was reused, with the reason in the `mode` column) and the
   resulting info-map size / error / warning counts, plus the triggering edit
   action. Read from PerfMetrics.frames, populated in CodeWithStatics.calculate
   while this panel is open. Implements DebugSection.S. */

let title = "Statics";

let row = (~max: Core.Time_ns.Span.t, f: PerfMetrics.frame): Node.t =>
  Node.tr([
    PerfFormat.action_cell(PerfFormat.action_of(f.perform)),
    PerfFormat.heat_cell(~max, ~cls=["perf-total"], f.statics),
    PerfFormat.int_cell(f.info_map_entries),
    PerfFormat.int_cell(f.errors),
    PerfFormat.int_cell(f.warnings),
    Node.td([text(f.statics_mode)]),
  ]);

let view = (~globals as _: Globals.t): list(Node.t) =>
  switch (PerfMetrics.frames^) {
  | [] => [
      PerfFormat.empty("No statics recorded yet — type in the editor."),
    ]
  | frames =>
    let max =
      PerfFormat.max_span(
        List.map((f: PerfMetrics.frame) => f.statics, frames),
      );
    [
      PerfFormat.table([
        PerfFormat.head_row([
          ("action", "The edit action that triggered this frame."),
          (
            "time",
            "Time to recompute statics + elaboration (Statics.mk). — when the recompute was skipped this frame.",
          ),
          (
            "entries",
            "Number of entries in the info map (one per expression id).",
          ),
          ("err", "Number of static error ids."),
          ("warn", "Number of static warning ids."),
          (
            "mode",
            "Whether statics recomputed this frame: recompute, forced, deferred (debounced), or cached (reused).",
          ),
        ]),
        ...List.map(row(~max), frames),
      ]),
    ];
  };
