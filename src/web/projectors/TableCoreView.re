open Virtual_dom.Vdom;
open Haz3lcore;
open ProjectorBase;
open Language;

/* Shared table cell/assembly rendering for TableProjView and
   TableRendererView (the Vdom half of TableCore, whose parsing logic
   stays in core). */

let value_view = (utility: utility, view_seg, exp) => {
  let (seg, _length) =
    ProbeUtil.abbreviated_seg_of(utility, TableCore.max_column_length, exp);

  Node.div(~attrs=[Attr.classes(["value"])], [view_seg(Sort.Exp, seg)]);
};

let row_cells = (utility: utility, view_seg, row: list(Exp.t)): list(Node.t) =>
  List.map(e => Node.td([value_view(utility, view_seg, e)]), row);

let table_view =
    (~header_cells: list(Node.t), ~rows: list(list(Node.t))): Node.t =>
  Node.table(
    ~attrs=[Attr.classes(["table"])],
    [
      Node.thead([Node.tr(header_cells)]),
      Node.tbody(List.map(r => Node.tr(r), rows)),
    ],
  );
