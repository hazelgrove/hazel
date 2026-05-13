open Util;
open Virtual_dom.Vdom;
open ProjectorBase;
open Language;

/* TableCore: Shared table rendering utilities for TableProj and TableRenderer */

/* --- Cell Rendering --- */

let max_column_length = 12;

let len_seg = (utility: utility, seg: Segment.t): int =>
  seg |> utility.seg_to_string |> String.length;

let seg_of_exp = (utility: utility, exp: Exp.t): (Segment.t, int) => {
  let seg = utility.term_to_seg(~inline=true, Exp(exp));
  (seg, len_seg(utility, seg));
};

let abbreviated_seg_of =
    (utility: utility, available: int, exp: Exp.t): (Segment.t, int) => {
  let (abbr_exp, _length) =
    exp
    |> DHExp.strip_ascriptions
    |> Exp.strip_projectors
    |> Abbreviate.abbreviate_exp(~available);
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

let value_view = (utility: utility, view_seg, exp) => {
  let (seg, length) = abbreviated_seg_of(utility, max_column_length, exp);

  Node.div(
    ~attrs=[Attr.classes(["value", length_cls(length)])],
    [view_seg(Sort.Exp, seg)],
  );
};

/* --- Table Assembly --- */

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

/* --- Table Parsing --- */

type table_data = (list(option(string)), list(list(Exp.t)));

let rec extract_entry = (e: Exp.t): option((option(string), Exp.t)) =>
  switch (e.term) {
  | Parens(inner) => extract_entry(inner)
  | TupLabel({term: Label(l), _}, v) => Some((Some(l), v))
  | TupLabel({term: EmptyHole, _}, v) => Some((None, v))
  | _ => None
  };

/* Peel Parens and push outer Asc wrappers into the tuple so labeled
 * entries surface in their normal shape. Revisit if elaboration changes
 * how it adds ascriptions to list rows. */
let rec normalize_row = (e: Exp.t): Exp.t =>
  switch (e.term) {
  | Parens(inner) => normalize_row(inner)
  | Asc(_, _) =>
    let stepped = Ascriptions.transition_multiple(e);
    stepped === e ? e : normalize_row(stepped);
  | _ => e
  };

let parse_table = (exp: Exp.t): option(table_data) =>
  switch (exp.term) {
  | ListLit(es) =>
    let data =
      List.map(
        (e: Exp.t) =>
          switch (normalize_row(e).term) {
          | Tuple(ds) =>
            OptUtil.traverse(extract_entry, ds) |> Option.map(List.split)
          | _ => None
          },
        es,
      );

    let data_opt = OptUtil.sequence(data);
    switch (data_opt) {
    | Some(data) =>
      let (headers, rows) = List.split(data);
      switch (headers) {
      | [] => None
      | [h, ..._]
          when
            List.for_all(List.equal(Option.equal(String.equal), h), headers) =>
        Some((h, rows))
      | _ => None
      };
    | None => None
    };
  | _ => None
  };
