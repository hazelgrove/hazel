/* A simple metavariable generator */
[@deriving sexp]
type t = MetaVar.t;
let init: t;
let next: t => (t, t);
