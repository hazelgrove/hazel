let precedence_bin_bool_op: DHExp.BinBoolOp.t => int;

let precedence_bin_int_op: DHExp.BinIntOp.t => int;

let precedence_bin_float_op: DHExp.BinFloatOp.t => int;

let precedence: (~show_casts: bool, DHExp.t) => int;

let mk_bin_bool_op: DHExp.BinBoolOp.t => DHDoc_common.t;

let mk_bin_int_op: DHExp.BinIntOp.t => DHDoc_common.t;

let mk_bin_float_op: DHExp.BinFloatOp.t => DHDoc_common.t;

let mk:
  (
    ~show_casts: bool,
    ~show_fn_bodies: bool,
    ~show_case_clauses: bool,
    ~parenthesize: bool=?,
    ~enforce_inline: bool,
    ~selected_instance: option(HoleInstance.t),
    DHExp.t
  ) =>
  DHDoc_common.t;

let mk_rule:
  (
    ~show_casts: bool,
    ~show_fn_bodies: bool,
    ~show_case_clauses: bool,
    ~selected_instance: option(HoleInstance.t),
    DHExp.rule
  ) =>
  DHDoc_common.t;
