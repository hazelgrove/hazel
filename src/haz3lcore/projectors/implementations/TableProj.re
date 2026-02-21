open Util;
open Virtual_dom.Vdom;
open ProjectorBase;
open Language;

let max_column_length = 12;

let rec extract_labeled_tuple_entries =
        (exp: Exp.t): option(list((LabeledTuple.label, DHExp.t))) => {
  switch (exp.term) {
  | Parens(e) => extract_labeled_tuple_entries(e)
  | Tuple(es) =>
    OptUtil.traverse(
      (e: Exp.t) => {
        switch (e.term) {
        | TupLabel({term: Label(l), _}, inner) => Some((l, inner))
        | _ => None
        }
      },
      es,
    )
  | _ => None
  };
};

let table_of =
    (any: Any.t): option((list(LabeledTuple.label), list(list(Exp.t)))) =>
  switch (any) {
  | Exp({term: ListLit(es), _}) =>
    switch (
      OptUtil.traverse(
        e => extract_labeled_tuple_entries(e) |> Option.map(List.split),
        es,
      )
    ) {
    | Some(data: list((list(string), list(TermBase.exp_t)))) =>
      let (headers: list(list(string)), rows: list(list(TermBase.exp_t))) =
        List.split(data);

      // If all the headers aren't the same return None
      switch (headers) {
      | [] => None
      | [h, ..._] when List.for_all(x => x == h, headers) =>
        let headers = h;
        Some((headers, rows));

      | _ => None
      };
    | None => None
    }
  | _ => None
  };

let get =
    (info: info): option((list(LabeledTuple.label), list(list(Exp.t)))) =>
  switch (info.syntax |> info.utility.seg_to_term) {
  | Some(s) => table_of(s)
  | None => None
  };

let len_seg = (utility: utility, seg: Segment.t): int =>
  seg |> utility.seg_to_string |> String.length;

let seg_of_exp = (utility: utility, exp: Exp.t): (Segment.t, int) => {
  let seg = utility.term_to_seg(~inline=true, Exp(exp));
  (seg, len_seg(utility, seg));
};

let abbreviated_seg_of =
    (utility: utility, available: int, exp: Exp.t): (Segment.t, int) => {
  let (abbr_exp, _length) =
    exp |> DHExp.strip_ascriptions |> Abbreviate.abbreviate_exp(~available);
  seg_of_exp(utility, abbr_exp);
};
let length_cls = (length: int): string =>
  if (length > 10) {
    "extra";
  } else if (length > 9) {
    "s6";
  } else if (length > 8) {
    "s5";
  } else if (length > 7) {
    "s4";
  } else if (length > 6) {
    "s3";
  } else if (length > 5) {
    "s2";
  } else if (length > 4) {
    "s1";
  } else {
    "s0";
  };
let value_view = (_info: info, utility: utility, view_seg, exp) => {
  let (seg, length) = abbreviated_seg_of(utility, max_column_length, exp);

  Node.div(
    ~attrs=[Attr.classes(["value", length_cls(length)])],
    [view_seg(Sort.Exp, seg)],
  );
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
                e => Node.td([value_view(info, info.utility, view_seg, e)]),
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
  let placeholder = (_, info) =>
    switch (get(info)) {
    | None =>
      ProjectorCore.Shape.{
        vertical: Inline,
        horizontal: 3,
      }
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
      View.mk(
        Node.div(
          ~attrs=[Attr.classes(["table", "error"])],
          [Node.text("\xe2\x9a\xa0")],
        ),
      )
    | Some(data) => View.mk(table(info, ~view_seg, ~parent, data))
    };
};
