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

/* --- Resize Machinery --- */

let min_width_blocks: int;
let min_height_blocks: int;
let clamp_width_blocks: int => int;
let clamp_height_blocks: int => int;

let default_size_for_table:
  (list(option(string)), list(list(Exp.t))) => (int, int);

let resize_handle:
  (
    ~dispatch: (int, int) => Virtual_dom.Vdom.Effect.t(unit),
    ~width_blocks: int,
    ~height_blocks: int
  ) =>
  Node.t;

/* --- Table Parsing --- */

type table_data = (list(option(string)), list(list(Exp.t)));
let parse_table: Exp.t => option(table_data);
