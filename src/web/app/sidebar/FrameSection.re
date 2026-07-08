open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;

/* The "Frame Timing" debug sidebar section: the per-keystroke timeline. One row
   per recent frame — the edit action's perform (update phase) then the
   calculate-phase stages (statics / syntax rebuild / cursor info / color maps)
   and the grand total — plus a rolling max of the total. A stage shows `—` when
   it didn't run that frame (e.g. statics deferred by the debounce). Read from
   PerfMetrics, populated across CodeEditable / CodeWithStatics / Page while this
   panel is open. Implements DebugSection.S. */

let title = "Frame Timing";

let row = (f: PerfMetrics.frame): Node.t =>
  Node.tr([
    PerfFormat.action_cell(PerfFormat.action_of(f.perform)),
    PerfFormat.span_cell(f.perform |> Option.map(snd)),
    PerfFormat.span_cell(f.statics),
    PerfFormat.span_cell(f.syntax),
    PerfFormat.span_cell(f.cursor_info),
    PerfFormat.span_cell(f.color_map),
    Node.td(
      ~attrs=[clss(["perf-total"])],
      [text(PerfFormat.span_str(f.total))],
    ),
  ]);

/* Rolling max of the total across the recorded frames. */
let max_total =
    (frames: list(PerfMetrics.frame)): option(Core.Time_ns.Span.t) => {
  let step =
      (acc: option(Core.Time_ns.Span.t), f: PerfMetrics.frame)
      : option(Core.Time_ns.Span.t) =>
    switch (acc, f.total) {
    | (None, t) => t
    | (Some(_) as a, None) => a
    | (Some(m), Some(t)) =>
      Some(Core.Time_ns.Span.compare(t, m) > 0 ? t : m)
    };
  List.fold_left(step, None, frames);
};

let view = (~globals as _: Globals.t): list(Node.t) =>
  switch (PerfMetrics.frames^) {
  | [] => [
      PerfFormat.empty("No frames recorded yet — type in the editor."),
    ]
  | frames => [
      PerfFormat.note(
        "max total: " ++ PerfFormat.span_str(max_total(frames)),
      ),
      PerfFormat.table([
        PerfFormat.head_row([
          "action",
          "perform",
          "statics",
          "syntax",
          "cursor",
          "colors",
          "total",
        ]),
        ...List.map(row, frames),
      ]),
    ]
  };
