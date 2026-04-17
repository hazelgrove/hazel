open Virtual_dom.Vdom;
open ProjectorBase;
open Language;

/* --- Cell Rendering --- */

let max_column_length: int;
let len_seg: (utility, Segment.t) => int;
let seg_of_exp: (utility, Exp.t) => (Segment.t, int);
let abbreviated_seg_of: (utility, int, Exp.t) => (Segment.t, int);
let length_cls: int => string;
let value_view: (utility, (Sort.t, Segment.t) => Node.t, Exp.t) => Node.t;

/* --- Table Assembly --- */

let row_cells:
  (utility, (Sort.t, Segment.t) => Node.t, list(Exp.t)) => list(Node.t);
let table_view:
  (~header_cells: list(Node.t), ~rows: list(list(Node.t))) => Node.t;

/* --- Table Parsing --- */

type table_data = (list(option(string)), list(list(Exp.t)));
let parse_table: Exp.t => option(table_data);
