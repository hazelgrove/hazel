/**
 * The most recently selected instance for each hole for which a selection
 * has been made.
 */
[@deriving sexp]
type t;

let init: t;
let find_opt: (int, t) => option(MetaVarInst.t);
let add: (int, MetaVarInst.t, t) => t;
