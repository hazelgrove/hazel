[@deriving sexp]
type t;

let init: t;

let next: t => (EnvironmentId.t, t);
