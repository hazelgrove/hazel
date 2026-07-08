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

let row = (f: PerfMetrics.frame): Node.t =>
  Node.tr([
    PerfFormat.action_cell(PerfFormat.action_of(f.perform)),
    Node.td(
      ~attrs=[clss(["perf-total"])],
      [text(PerfFormat.span_str(f.statics))],
    ),
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
  | frames => [
      PerfFormat.table([
        PerfFormat.head_row([
          "action",
          "time",
          "entries",
          "err",
          "warn",
          "mode",
        ]),
        ...List.map(row, frames),
      ]),
    ]
  };
