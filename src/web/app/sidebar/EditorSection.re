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

let columns =
    (~max: Core.Time_ns.Span.t): list(PerfFormat.column(PerfMetrics.frame)) => [
  PerfFormat.action_column((f: PerfMetrics.frame) =>
    PerfFormat.fmt_opt(fst, f.perform)
  ),
  {
    label: "rebuild",
    tooltip: "Time to rebuild the syntax cache: MakeTerm (segment → term) + Measured (layout).",
    cell: f => PerfFormat.heat_cell(~max, ~total=true, f.syntax),
  },
  {
    label: "tokens",
    tooltip: "Number of pieces (tokens) in the editor segment.",
    cell: f => PerfFormat.int_cell(f.segment_tokens),
  },
  {
    label: "tiles",
    tooltip: "Number of distinct tiles in the measured layout.",
    cell: f => PerfFormat.int_cell(f.tiles),
  },
  {
    label: "rows",
    tooltip: "Number of rendered rows in the measured layout.",
    cell: f => PerfFormat.int_cell(f.rows),
  },
  {
    label: "proj",
    tooltip: "Number of projectors in the segment.",
    cell: f => PerfFormat.int_cell(f.projectors),
  },
];

let view = (~globals as _: Globals.t): list(Node.t) => {
  let live = PerfMetrics.live^;
  let undo_redo =
    PerfFormat.note(
      Printf.sprintf(
        "undo %d · redo %d · backpack %d",
        live.undo_depth,
        live.redo_depth,
        live.backpack,
      ),
    );
  switch (PerfMetrics.history^) {
  | [] => [
      undo_redo,
      PerfFormat.empty("No edits recorded yet — type in the editor."),
    ]
  | frames =>
    let max =
      frames
      |> List.to_seq
      |> Seq.map((f: PerfMetrics.frame) => f.syntax)
      |> PerfFormat.max_span;
    [
      undo_redo,
      PerfFormat.table(
        ~columns=columns(~max),
        List.map(f => PerfFormat.Row(f), frames),
      ),
    ];
  };
};
