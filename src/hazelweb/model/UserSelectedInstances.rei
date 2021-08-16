/**
 * The most recently selected instance for each hole for which a selection
 * has been made.
 */
[@deriving (sexp, show)]
type t;

let init: t;
let find_opt: (MetaVar.t, t) => option(MetaVarInst.t);
let add: (MetaVar.t, MetaVarInst.t, t) => t;
