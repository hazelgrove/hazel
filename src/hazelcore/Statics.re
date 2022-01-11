[@deriving sexp]
type section_edit_state = (ZExp.t, HTyp.t, IDGen.t);

[@deriving sexp]
type edit_state = {
  prelude: section_edit_state,
  template: section_edit_state,
  tester: section_edit_state,
};

/**
 * The typing mode for some subexpression in the program
 */
type type_mode =
  | Syn
  | Ana(HTyp.t);

let merge_edit_states =
    (lhs: section_edit_state, rhs: section_edit_state): section_edit_state => {
  assert(false);
};
