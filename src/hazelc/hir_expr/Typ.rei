[@deriving sexp]
type t =
  | Hole
  | Int
  | Float
  | Bool
  | Arrow(t, t)
  | Sum(t, t)
  | Prod(list(t))
  | List(t);

let equal: (t, t) => bool;

let consistent: (t, t) => bool;
