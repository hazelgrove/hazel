open Virtual_dom.Vdom;

/* The "Editor & Memory" debug sidebar section: a per-frame history of the
   syntax-cache rebuild (MakeTerm + Measured) — how long each took and the
   resulting structural counts (segment tokens, tiles, rows, projectors) — with
   the triggering edit action, plus a live undo/redo/backpack line. Read from
   PerfMetrics, populated in CodeWithStatics.calculate / History while this
   panel is open. Byte-exact sizes are intentionally not computed here (heap
   walks are expensive; we're after timings + counts). Implements
   DebugSection.S. */

let title = "Editor & Memory";

let row = (~max: Core.Time_ns.Span.t, f: PerfMetrics.frame): Node.t =>
  Node.tr([
    PerfFormat.action_cell(PerfFormat.action_of(f.perform)),
    PerfFormat.heat_cell(~max, ~cls=["perf-total"], f.syntax),
    PerfFormat.int_cell(f.segment_tokens),
    PerfFormat.int_cell(f.tiles),
    PerfFormat.int_cell(f.rows),
    PerfFormat.int_cell(f.projectors),
  ]);

let view = (~globals as _: Globals.t): list(Node.t) => {
  let c = PerfMetrics.counts;
  let undo_redo =
    PerfFormat.note(
      Printf.sprintf(
        "undo %d · redo %d · backpack %d",
        c.undo_depth,
        c.redo_depth,
        c.backpack,
      ),
    );
  switch (PerfMetrics.frames^) {
  | [] => [
      undo_redo,
      PerfFormat.empty("No edits recorded yet — type in the editor."),
    ]
  | frames =>
    let max =
      PerfFormat.max_span(
        List.map((f: PerfMetrics.frame) => f.syntax, frames),
      );
    [
      undo_redo,
      PerfFormat.table([
        PerfFormat.head_row([
          ("action", "The edit action that triggered this frame."),
          (
            "rebuild",
            "Time to rebuild the syntax cache: MakeTerm (segment → term) + Measured (layout).",
          ),
          ("tokens", "Number of pieces (tokens) in the editor segment."),
          ("tiles", "Number of distinct tiles in the measured layout."),
          ("rows", "Number of rendered rows in the measured layout."),
          ("proj", "Number of projectors in the segment."),
        ]),
        ...List.map(row(~max), frames),
      ]),
    ];
  };
};
