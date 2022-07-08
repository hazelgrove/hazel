[@deriving sexp]
type t;

let init: t;

let next: t => (Label.t, t);
