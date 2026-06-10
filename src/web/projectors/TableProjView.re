open Virtual_dom.Vdom;
open Haz3lcore;
open ProjectorBase;
open ProjectorViewBase;
open Language;

let table =
    (
      info,
      ~parent as _: external_action => Ui_effect.t(unit),
      (headers, rows): (list(LabeledTuple.label), list(list(Exp.t))),
      ~view_seg: (Sort.t, Segment.t) => Node.t,
    ) =>
  TableCoreView.table_view(
    ~header_cells=List.map(h => Node.th([Node.text(h)]), headers),
    ~rows=List.map(TableCoreView.row_cells(info.utility, view_seg), rows),
  );

module V: ProjectorView = {
  module L = TableProj.M;

  let focusable =
    Focusable.{
      pointer: None,
      keyboard: None,
    };

  let view =
      ({info, parent, view_seg, _}: View.args(L.model, L.action)): View.t =>
    switch (TableProj.get(info)) {
    | None =>
      let seg = Segment.unparenthesize(info.syntax);
      let sort = Segment.sort_of(Segment.skel(seg), seg);
      let banner =
        Node.div(
          ~attrs=[Attr.classes(["table-error-banner"])],
          [Node.text(TableProj.error_message)],
        );
      View.mk(
        ~error=true,
        Node.div(
          ~attrs=[Attr.classes(["table-inner"])],
          [banner, view_seg(sort, seg)],
        ),
      );
    | Some(data) =>
      View.mk(
        Node.div(
          ~attrs=[Attr.classes(["table-inner"])],
          [table(info, ~view_seg, ~parent, data)],
        ),
      )
    };
};
