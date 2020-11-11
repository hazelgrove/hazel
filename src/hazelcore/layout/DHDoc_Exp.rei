let mk:
  (
    ~show_casts: bool,
    ~show_fn_bodies: bool,
    ~show_case_clauses: bool,
    ~parenthesize: bool=?,
    ~enforce_inline: bool,
    ~selected_hole_instance: option(NodeInstance.t),
    DHExp.t
  ) =>
  DHDoc_common.t;
