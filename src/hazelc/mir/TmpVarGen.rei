[@deriving sexp]
type t;

let init: t;

let next: t => (Var.t, t);

let next_named: (Var.t, t) => (Var.t, t);
