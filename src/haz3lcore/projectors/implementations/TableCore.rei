open Virtual_dom.Vdom;
open ProjectorBase;
open Language;

/* --- Cell Rendering --- */

let max_column_length: int;
let value_view: (utility, (Sort.t, Segment.t) => Node.t, Exp.t) => Node.t;

/* One flag per column: every cell is a number literal. */
let numeric_columns: list(list(Exp.t)) => list(bool);
/* The `numeric` class for column i's header and cells, when it is numeric. */
let numeric_attrs: (list(bool), int) => list(Attr.t);

/* --- Table Assembly --- */

let row_cells:
  (
    ~numeric: list(bool)=?,
    utility,
    (Sort.t, Segment.t) => Node.t,
    list(Exp.t)
  ) =>
  list(Node.t);
let table_view:
  (~header_cells: list(Node.t), ~rows: list(list(Node.t))) => Node.t;

/* --- Table Parsing --- */

type table_data = (list(option(string)), list(list(Exp.t)));
let parse_table: Exp.t => option(table_data);
