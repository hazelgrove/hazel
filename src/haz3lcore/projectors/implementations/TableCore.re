open Util;
open Virtual_dom.Vdom;
open ProjectorBase;
open Language;

/* TableCore: Shared table rendering utilities for TableProj and TableRenderer */

/* --- Cell Rendering --- */

/* Per-cell abbreviation budget, in characters.
 *
 * Only cells longer than this abbreviate, so raising it widens nothing that
 * already fits -- names and numbers are unaffected. It was 12, which is
 * enough for a word or a number but not for a constructor with a payload:
 * an adapton event reads "AddNode(Symbol(\"g\"), Now, 1)", and at 12 every
 * such cell came out as "AddNode(...+3)", which says nothing. */
let max_column_length = 32;

let value_view = (utility: utility, view_seg, exp) => {
  let (seg, _length) =
    ProbeUtil.abbreviated_seg_of(utility, max_column_length, exp);

  Node.div(~attrs=[Attr.classes(["value"])], [view_seg(Sort.Exp, seg)]);
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
 * how it adds ascriptions to list rows.
 *
 * Used on the list itself as well as on its rows: a table whose list is
 * ascribed -- (e : [Row]) -- is the ordinary way to write one whose
 * elements a reader cannot infer, and it is the only way to write one that
 * comes from a livelit, since those expand only in checking mode. */
let rec normalize_row = (e: Exp.t): Exp.t =>
  switch (e.term) {
  | Parens(inner) => normalize_row(inner)
  | Asc(_, _) =>
    let stepped = Ascriptions.transition_multiple(e);
    stepped === e ? e : normalize_row(stepped);
  | _ => e
  };

let parse_table = (exp: Exp.t): option(table_data) =>
  switch (normalize_row(exp).term) {
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
