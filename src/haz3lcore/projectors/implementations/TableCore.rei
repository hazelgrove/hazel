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

/* --- Table Parsing --- */

type table_data = (list(option(string)), list(list(Exp.t)));
let parse_table: Exp.t => option(table_data);
