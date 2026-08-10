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

/* --- Row/Column Editing ---
 *
 * Piece-level transformations over a spliced table literal, used by
 * the context-menu actions the table projector contributes inside its
 * cell splices. All of them preserve the untouched pieces (including
 * cell splices) by identity, so live sub-editors carry over. */

let insert_nth = (n: int, x: 'a, xs: list('a)): list('a) => {
  let (l, r) = ListUtil.split_n(min(n, List.length(xs)), xs);
  l @ [x, ...r];
};

let remove_nth = (n: int, xs: list('a)): list('a) => {
  let (l, r) = ListUtil.split_n(n, xs);
  switch (r) {
  | [] => l
  | [_, ...r] => l @ r
  };
};

let comma_tile = (): Base.piece => Piece.mk_tile(Form.get(CommaExp), []);
let eq_tile = (): Base.piece => Piece.mk_tile(Form.get(TupleLabeledExp), []);
let label_tile = (tok: string): Base.piece =>
  Piece.mk_tile(Form.mk_atom(tok, Mold.mk_op(Exp)), []);
let space_piece = (): Base.piece =>
  Base.Secondary(
    Language.Secondary.{
      content: Whitespace(" "),
      id: Id.mk(),
    },
  );
let hole_splice = (): Base.piece =>
  Piece.mk_splice([Piece.mk_grout(Convex)]);

/* Copies of a group's leading secondary with fresh ids, so a new row
 * can inherit the formatting (newline + indent) of an existing one. */
let fresh_lead_secondary = (group: Base.segment): Base.segment => {
  let (lead, _, _) = split_outer_secondary(group);
  List.filter_map(
    (p: Base.piece) =>
      switch (p) {
      | Secondary(w) =>
        Some(
          Base.Secondary({
            ...w,
            id: Id.mk(),
          }),
        )
      | _ => None
      },
    lead,
  );
};

/* The items segment of the (possibly parenthesized) list tile at the
 * core of [seg]. */
let rec list_items = (seg: Base.segment): option(Base.segment) => {
  let (_, core, _) = split_outer_secondary(seg);
  switch (core) {
  | [Tile({label: ["(", ")"], children: [child], _})] => list_items(child)
  | [Tile({label: ["[", "]"], children: [items], _})] => Some(items)
  | _ => None
  };
};

/* Rebuild [seg] with its list-items segment mapped through [f]. */
let rec map_list_items =
        (f: Base.segment => option(Base.segment), seg: Base.segment)
        : option(Base.segment) => {
  open Util.OptUtil.Syntax;
  let (lead, core, trail) = split_outer_secondary(seg);
  switch (core) {
  | [Tile({label: ["(", ")"], children: [child], _} as t)] =>
    let+ child = map_list_items(f, child);
    lead
    @ [
      Base.Tile({
        ...t,
        children: [child],
      }),
    ]
    @ trail;
  | [Tile({label: ["[", "]"], children: [items], _} as t)] =>
    let+ items = f(items);
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

/* The cell segments of one row (a parenthesized tuple), including
 * each cell's surrounding secondary. */
let row_cell_segs = (row: Base.segment): option(list(Base.segment)) =>
  switch (split_outer_secondary(row)) {
  | (_, [Tile({label: ["(", ")"], children: [tuple_child], _})], _) =>
    Some(Segment.split_at_commas(tuple_child) |> Aba.get_as)
  | _ => None
  };

/* Rebuild one row around a new list of cell segments. */
let rebuild_row =
    (row: Base.segment, cells: list(Base.segment)): option(Base.segment) =>
  switch (split_outer_secondary(row)) {
  | (lead, [Tile({label: ["(", ")"], children: [_], _} as t)], trail) =>
    let (_, commas) = Segment.split_at_commas(t.children |> List.hd);
    let n_commas = List.length(cells) - 1;
    let commas = {
      let kept = ListUtil.take(min(n_commas, List.length(commas)), commas);
      kept @ List.init(n_commas - List.length(kept), _ => comma_tile());
    };
    let tuple_child =
      Aba.mk(cells, commas) |> Aba.join(Fun.id, p => [p]) |> List.concat;
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

/* The label token of a cell, if it has a `label =` prefix. */
let cell_label = (cell: Base.segment): option(string) => {
  open Util.OptUtil.Syntax;
  let* (prefix, _) = split_at_label_sep(cell);
  List.find_map(
    (p: Base.piece) =>
      switch (p) {
      | Tile({label: [tok], children: [], _}) when tok != "=" => Some(tok)
      | _ => None
      },
    prefix,
  );
};

/* The splice id of a cell's value, if the value is a single splice. */
let cell_splice = (cell: Base.segment): option(Id.t) => {
  let value =
    switch (split_at_label_sep(cell)) {
    | Some((_, value)) => value
    | None => cell
    };
  switch (split_outer_secondary(value)) {
  | (_, [Splice(s)], _) => Some(s.id)
  | _ => None
  };
};

/* A fresh empty cell: `label=?` (label optional), with the hole
 * wrapped in a splice so it is editable in place. */
let mk_cell = (label: option(string)): Base.segment =>
  switch (label) {
  | Some(tok) => [label_tile(tok), eq_tile(), hole_splice()]
  | None => [hole_splice()]
  };

[@deriving (show({with_path: false}), sexp, yojson)]
type cell_pos = {
  row: int,
  col: int,
  n_rows: int,
  n_cols: int,
};

/* Locate the cell hosting splice [splice_id] in a table segment. */
let find_cell = (seg: Base.segment, splice_id: Id.t): option(cell_pos) => {
  open Util.OptUtil.Syntax;
  let* items = list_items(seg);
  let rows = Segment.split_at_commas(items) |> Aba.get_as;
  let n_rows = List.length(rows);
  List.mapi((i, row) => (i, row), rows)
  |> List.find_map(((i, row)) => {
       let* cells = row_cell_segs(row);
       let+ j =
         List.mapi((j, cell) => (j, cell), cells)
         |> List.find_map(((j, cell)) =>
              cell_splice(cell) == Some(splice_id) ? Some(j) : None
            );
       {
         row: i,
         col: j,
         n_rows,
         n_cols: List.length(cells),
       };
     });
};

/* The label structure of row [template] (per-cell label tokens). */
let row_labels = (seg: Base.segment, template: int): list(option(string)) => {
  switch (list_items(seg)) {
  | None => []
  | Some(items) =>
    let rows = Segment.split_at_commas(items) |> Aba.get_as;
    switch (List.nth_opt(rows, template)) {
    | None => []
    | Some(row) =>
      row_cell_segs(row)
      |> Option.map(List.map(cell_label))
      |> Option.value(~default=[])
    };
  };
};

/* Insert an empty row at index [at] (0 <= at <= n_rows), copying the
 * label structure of row [template] and the leading formatting of the
 * row currently at the insertion point. */
let insert_row =
    (seg: Base.segment, ~at: int, ~template: int): option(Base.segment) =>
  map_list_items(
    items => {
      open Util.OptUtil.Syntax;
      let (rows, commas) = Segment.split_at_commas(items);
      let n = List.length(rows);
      let* template_row = List.nth_opt(rows, min(template, n - 1));
      let* labels =
        row_cell_segs(template_row) |> Option.map(List.map(cell_label));
      let lead =
        switch (List.nth_opt(rows, min(at, n - 1))) {
        | Some(row) => fresh_lead_secondary(row)
        | None => []
        };
      let cells = List.map(mk_cell, labels);
      let rec join_cells = (cs: list(Base.segment)) =>
        switch (cs) {
        | [] => []
        | [c] => c
        | [c, ...rest] =>
          c @ [comma_tile(), space_piece()] @ join_cells(rest)
        };
      let new_row =
        lead @ [Piece.mk_tile(Form.get(ParensExp), [join_cells(cells)])];
      let rows = insert_nth(at, new_row, rows);
      let commas = commas @ [comma_tile()];
      Some(
        Aba.mk(rows, commas) |> Aba.join(Fun.id, p => [p]) |> List.concat,
      );
    },
    seg,
  );

/* Remove the row at index [at]. Declines when it is the only row. */
let remove_row = (seg: Base.segment, ~at: int): option(Base.segment) =>
  map_list_items(
    items => {
      let (rows, commas) = Segment.split_at_commas(items);
      let n = List.length(rows);
      if (n <= 1 || at >= n) {
        None;
      } else {
        let rows = remove_nth(at, rows);
        let commas = ListUtil.take(n - 2, commas);
        /* Dropping row 0 can leave the new first row with a leading
         * newline inherited from its old position; keep it — it only
         * affects unprojected formatting. */
        Some(
          Aba.mk(rows, commas) |> Aba.join(Fun.id, p => [p]) |> List.concat,
        );
      };
    },
    seg,
  );

/* Insert an empty cell at index [at] in every row (labeled [label]). */
let insert_col =
    (seg: Base.segment, ~at: int, ~label: option(string))
    : option(Base.segment) =>
  map_list_items(
    items => {
      open Util.OptUtil.Syntax;
      let (rows, commas) = Segment.split_at_commas(items);
      let+ rows =
        rows
        |> Util.OptUtil.traverse(row => {
             let* cells = row_cell_segs(row);
             let new_cell =
               (at == 0 ? [] : [space_piece()]) @ mk_cell(label);
             let cells =
               insert_nth(min(at, List.length(cells)), new_cell, cells);
             /* An insertion at the front leaves the old first cell
              * without a separating space after the new comma. */
             let cells =
               at == 0
                 ? switch (cells) {
                   | [c0, c1, ...rest] => [
                       c0,
                       [space_piece(), ...c1],
                       ...rest,
                     ]
                   | cells => cells
                   }
                 : cells;
             rebuild_row(row, cells);
           });
      Aba.mk(rows, commas) |> Aba.join(Fun.id, p => [p]) |> List.concat;
    },
    seg,
  );

/* Remove the cell at index [at] from every row. Declines when rows
 * have a single column. */
let remove_col = (seg: Base.segment, ~at: int): option(Base.segment) =>
  map_list_items(
    items => {
      open Util.OptUtil.Syntax;
      let (rows, commas) = Segment.split_at_commas(items);
      let+ rows =
        rows
        |> Util.OptUtil.traverse(row => {
             let* cells = row_cell_segs(row);
             let n = List.length(cells);
             if (n <= 1 || at >= n) {
               None;
             } else {
               let cells = remove_nth(at, cells);
               /* Removing the first cell strands its successor's
                * leading space. */
               let cells =
                 at == 0
                   ? switch (cells) {
                     | [c0, ...rest] => [
                         c0 |> Segment.trim_secondary(Util.Direction.Left),
                         ...rest,
                       ]
                     | cells => cells
                     }
                   : cells;
               rebuild_row(row, cells);
             };
           });
      Aba.mk(rows, commas) |> Aba.join(Fun.id, p => [p]) |> List.concat;
    },
    seg,
  );

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
