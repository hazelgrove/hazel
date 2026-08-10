open Util;
open Virtual_dom.Vdom;
open ProjectorBase;
open Language;

/* TableCore: Shared table rendering utilities for TableProj and TableRenderer */

/* --- Cell Rendering --- */

let max_column_length = 12;

let value_view = (utility: utility, view_seg, exp) => {
  let (seg, _length) =
    ProbeUtil.abbreviated_seg_of(utility, max_column_length, exp);

  Node.div(~attrs=[Attr.classes(["value"])], [view_seg(Sort.Exp, seg)]);
};

/* --- Table Assembly --- */

/* Table cells: [splice_cell] may claim a cell (e.g. rendering an
 * editable sub-editor for its splice); unclaimed cells render as
 * read-only abbreviated values. */
let row_cells =
    (
      utility: utility,
      view_seg,
      ~splice_cell: Exp.t => option(Node.t)=_ => None,
      row: list(Exp.t),
    )
    : list(Node.t) =>
  List.map(
    e =>
      switch (splice_cell(e)) {
      | Some(cell) => Node.td([cell])
      | None => Node.td([value_view(utility, view_seg, e)])
      },
    row,
  );

let table_view =
    (~header_cells: list(Node.t), ~rows: list(list(Node.t))): Node.t =>
  Node.table(
    ~attrs=[Attr.classes(["table"])],
    [
      Node.thead([Node.tr(header_cells)]),
      Node.tbody(List.map(r => Node.tr(r), rows)),
    ],
  );

/* --- Splice Cells ---
 *
 * Piece-level transformation wrapping each cell value of a table
 * literal in a splice, so the table projector can host an editable
 * sub-editor per cell. This works on the raw syntax (rather than
 * going through a term round-trip like VListProj) so the user's
 * formatting and piece ids are preserved exactly, and so it applies
 * even when table shape can only be validated against the elaborated
 * form (auto-labeled tuples). Splices are transparent at
 * term-construction and elaboration time, so headers can still be
 * inferred from the elaborated form and each elaborated cell carries
 * its splice wrapper (and id) along, even when elaboration reorders
 * labeled entries. */

/* Split a segment into (leading secondary, core, trailing secondary). */
let split_outer_secondary =
    (seg: Base.segment): (Base.segment, Base.segment, Base.segment) => {
  let (lead, rest) = Segment.take_while_secondary(seg);
  let (rev_trail, rev_core) = Segment.take_while_secondary(List.rev(rest));
  (lead, List.rev(rev_core), List.rev(rev_trail));
};

/* Split a cell segment at a top-level tuple-label separator
 * (TupleLabeled* infix "="), returning the prefix through the "="
 * tile and the value pieces after it. */
let split_at_label_sep =
    (cell: Base.segment): option((Base.segment, Base.segment)) => {
  let rec go = (prefix, ps: Base.segment) =>
    switch (ps) {
    | [] => None
    | [Base.Tile({label: ["="], _}) as eq, ...rest] =>
      Some((List.rev([eq, ...prefix]), rest))
    | [p, ...rest] => go([p, ...prefix], rest)
    };
  go([], cell);
};

/* Wrap the value part of one cell segment in a splice, leaving any
 * `label =` prefix and surrounding secondary outside the splice.
 * Idempotent: an already-spliced value is left as-is, so re-running
 * on spliced syntax (projector re-init) is a no-op. */
let wrap_cell_value = (cell: Base.segment): Base.segment => {
  let (label_prefix, value) =
    switch (split_at_label_sep(cell)) {
    | Some((prefix, value)) => (prefix, value)
    | None => ([], cell)
    };
  let (lead, core, trail) = split_outer_secondary(value);
  switch (core) {
  | []
  | [Splice(_)] => cell
  | _ => label_prefix @ lead @ [Piece.mk_splice(core)] @ trail
  };
};

let map_comma_groups =
    (f: Base.segment => Base.segment, seg: Base.segment): Base.segment =>
  Segment.split_at_commas(seg)
  |> Aba.map_a(f)
  |> Aba.join(Fun.id, p => [p])
  |> List.concat;

/* Wrap each cell value of one row — a parenthesized tuple — in a
 * splice. Returns None if the row isn't syntactically a tuple. */
let wrap_row_cells = (row: Base.segment): option(Base.segment) => {
  let (lead, core, trail) = split_outer_secondary(row);
  switch (core) {
  | [Tile({label: ["(", ")"], children: [tuple_child], _} as t)] =>
    let tuple_child = map_comma_groups(wrap_cell_value, tuple_child);
    Some(
      lead
      @ [
        Base.Tile({
          ...t,
          children: [tuple_child],
        }),
      ]
      @ trail,
    );
  | _ => None
  };
};

/* Wrap every cell value of a table literal segment in a splice.
 * Returns None if the syntax isn't a list literal of parenthesized
 * tuples — callers should fall back to a read-only rendering. */
let rec splice_table_cells = (seg: Base.segment): option(Base.segment) => {
  open Util.OptUtil.Syntax;
  let (lead, core, trail) = split_outer_secondary(seg);
  switch (core) {
  | [Tile({label: ["(", ")"], children: [child], _} as t)] =>
    let+ child = splice_table_cells(child);
    lead
    @ [
      Base.Tile({
        ...t,
        children: [child],
      }),
    ]
    @ trail;
  | [Tile({label: ["[", "]"], children: [items], _} as t)] =>
    let (rows, commas) = Segment.split_at_commas(items);
    let+ rows = Util.OptUtil.traverse(wrap_row_cells, rows);
    let items =
      Aba.mk(rows, commas) |> Aba.join(Fun.id, p => [p]) |> List.concat;
    lead
    @ [
      Base.Tile({
        ...t,
        children: [items],
      }),
    ]
    @ trail;
  | _ => None
  };
};

/* The id of the outermost splice term in a cell expression, if any.
 * Elaboration preserves splice wrappers (and their ids), so this maps
 * an elaborated cell back to the splice piece in the projector's
 * syntax even when elaboration reorders or relabels entries. */
let first_splice_id = (e: Exp.t): option(Id.t) => {
  module M = {
    exception Found(Id.t);
  };
  switch (
    Exp.map_term(
      ~f_exp=
        (cont, e) =>
          switch (e.term) {
          | Splice(_) => raise(M.Found(Exp.rep_id(e)))
          | _ => cont(e)
          },
      e,
    )
  ) {
  | exception (M.Found(id)) => Some(id)
  | _ => None
  };
};

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
