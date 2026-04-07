open Util;
open Virtual_dom.Vdom;
open ProjectorBase;
open Language;
open TableCore;

let table_of =
    (any: Any.t): option((list(LabeledTuple.label), list(list(Exp.t)))) =>
  switch (any) {
  | Exp(exp) =>
    parse_table(exp)
    |> Option.bind(_, ((headers, rows)) =>
         OptUtil.traverse(Fun.id, headers) |> Option.map(hs => (hs, rows))
       )
  | _ => None
  };

let get =
    (info: info): option((list(LabeledTuple.label), list(list(Exp.t)))) =>
  switch (info.elaborated) {
  | Some(elab_exp) => table_of(Exp(elab_exp))
  | None =>
    switch (info.syntax |> info.utility.seg_to_term) {
    | Some(s) => table_of(s)
    | None => None
    }
  };

let table =
    (
      info,
      ~parent as _: external_action => Ui_effect.t(unit),
      (headers, rows): (list(LabeledTuple.label), list(list(Exp.t))),
      ~view_seg: (Sort.t, Segment.t) => Node.t,
    ) =>
  Node.table(
    ~attrs=[Attr.classes(["table"])],
    [
      Node.thead([
        Node.tr(List.map(h => Node.th([Node.text(h)]), headers)),
      ]),
      Node.tbody(
        List.map(
          row =>
            Node.tr(
              List.map(
                e => Node.td([value_view(info.utility, view_seg, e)]),
                row,
              ),
            ),
          rows,
        ),
      ),
    ],
  );

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = unit;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = unit;

  let init = (any: Any.t) =>
    switch (table_of(any)) {
    | Some(_) => Some()
    | None => None
    };

  let focusable =
    Focusable.{
      pointer: None,
      keyboard: None,
    };
  let dynamics = false;
  let elaborate_syntax = true;
  let placeholder = (_, info) =>
    switch (get(info)) {
    | None =>
      let s = info.utility.seg_to_string(info.syntax);
      let lines = String.split_on_char('\n', s);
      let n_lines = List.length(lines);
      let max_width =
        List.fold_left(
          (acc, line) => max(acc, String.length(line)),
          0,
          lines,
        );
      ProjectorCore.Shape.{
        vertical: n_lines <= 1 ? Inline : Block(n_lines - 1),
        horizontal: max_width,
      };
    | Some((header, rows)) =>
      let max_header_length =
        header |> List.map(String.length) |> List.fold_left((+), 0);
      let max_row_length =
        rows
        |> List.map(row =>
             row
             |> List.map(e =>
                  Abbreviate.abbreviate_exp(~available=max_column_length, e)
                  |> snd
                )
             |> List.fold_left((+), 0, _)
           )
        |> List.fold_left(max, 0, _);
      let max_length = max(max_header_length, max_row_length);

      let num_rows = List.length(rows);
      let num_cols = List.length(header);
      ProjectorCore.Shape.{
        vertical: Block(min(num_rows, 10)),
        horizontal: 4 + max_length * 1 + num_cols * 2,
      };
    };
  let update = (model, _, _) => model;

  let view = ({info, parent, view_seg, _}: View.args(model, action)): View.t =>
    switch (get(info)) {
    | None =>
      let seg = Segment.unparenthesize(info.syntax);
      let sort = Segment.sort_of(Segment.skel(seg), seg);
      View.mk(
        ~error=true,
        Node.div(
          ~attrs=[Attr.classes(["table-inner"])],
          [view_seg(sort, seg)],
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
