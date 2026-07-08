open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;

/* The "Editor & Memory" debug sidebar section: a per-frame history of the
   syntax-cache rebuild (MakeTerm + Measured) — how long each took and the
   resulting structural counts (segment tokens, tiles, rows, projectors) — with
   the triggering edit action, plus a live undo/redo/backpack line. Read from
   PerfMetrics, populated in CodeWithStatics.calculate / History while this
   panel is open. Byte-exact sizes are intentionally not computed here (heap
   walks are expensive; we're after timings + counts). Implements
   DebugSection.S. */

let title = "Editor & Memory";

let row = (f: PerfMetrics.frame): Node.t =>
  Node.tr([
    PerfFormat.action_cell(PerfFormat.action_of(f.perform)),
    Node.td(
      ~attrs=[clss(["perf-total"])],
      [text(PerfFormat.span_str(f.syntax))],
    ),
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
  | frames => [
      undo_redo,
      PerfFormat.table([
        PerfFormat.head_row([
          "action",
          "rebuild",
          "tokens",
          "tiles",
          "rows",
          "proj",
        ]),
        ...List.map(row, frames),
      ]),
    ]
  };
};
