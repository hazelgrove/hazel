/**
 * The most recently selected instance for each hole for which a selection
 * has been made.
 */
[@deriving sexp]
type t;

let init: t;
let find_opt:
  (TaggedNodeInstance.kind, MetaVar.t, t) => option(MetaVarInst.t);
let add: ((TaggedNodeInstance.kind, (MetaVar.t, MetaVarInst.t)), t) => t;
