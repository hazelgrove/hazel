/* A simple metavariable generator */
[@deriving (sexp, show)]
type t = MetaVar.t;
let init: t;
let next: t => (t, t);
