open Virtual_dom.Vdom;
open ProjectorBase;
open Language;

/* --- Cell Rendering --- */

let max_column_length: int;
let value_view: (utility, (Sort.t, Segment.t) => Node.t, Exp.t) => Node.t;

/* --- Table Assembly --- */

let row_cells:
  (
    utility,
    (Sort.t, Segment.t) => Node.t,
    ~splice_cell: Exp.t => option(Node.t)=?,
    list(Exp.t)
  ) =>
  list(Node.t);
let table_view:
  (~header_cells: list(Node.t), ~rows: list(list(Node.t))) => Node.t;

/* --- Splice Cells --- */

/* Wrap every cell value of a table-literal segment in a splice (piece
 * level, preserving formatting and ids; idempotent). None if the
 * syntax isn't a list literal of parenthesized tuples. */
let splice_table_cells: Base.segment => option(Base.segment);

/* The id of the outermost splice term in a cell expression, if any. */
let first_splice_id: Exp.t => option(Id.t);

/* --- Row/Column Editing ---
 * Piece-level edits over a spliced table literal; None when the
 * syntax doesn't have the expected list-of-tuples shape (or the edit
 * would leave the table without any row/column). */

[@deriving (show({with_path: false}), sexp, yojson)]
type cell_pos = {
  row: int,
  col: int,
  n_rows: int,
  n_cols: int,
};

/* Locate the cell hosting a given splice. */
let find_cell: (Base.segment, Id.t) => option(cell_pos);

/* Per-cell label tokens of the row at the given index. */
let row_labels: (Base.segment, int) => list(option(string));

let insert_row:
  (Base.segment, ~at: int, ~template: int) => option(Base.segment);
let remove_row: (Base.segment, ~at: int) => option(Base.segment);
let insert_col:
  (Base.segment, ~at: int, ~label: option(string)) => option(Base.segment);
let remove_col: (Base.segment, ~at: int) => option(Base.segment);

/* Labels carried syntactically by every row (renameable columns). */
let renameable_labels: Base.segment => list(string);

/* Rename a label in every row's `label =` prefix. */
let rename_label:
  (Base.segment, ~from: string, ~to_: string) => option(Base.segment);

/* --- Table Parsing --- */

type table_data = (list(option(string)), list(list(Exp.t)));
let parse_table: Exp.t => option(table_data);
