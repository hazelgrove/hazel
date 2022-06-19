[@deriving sexp]
type edit_state = (ZExp.t, HTyp.t, IDGen.t);

/**
 * The typing mode for some subexpression in the program
 */
type type_mode =
  | Syn
  | Ana(HTyp.t);
