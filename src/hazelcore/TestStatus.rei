[@deriving sexp]
type t =
  | Pass
  | Fail
  | Indet;

let to_string: t => string;

let join: (t, t) => t;
let join_all: list(t) => t;
