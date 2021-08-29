[@deriving sexp]
type t =
  | Typ
  | Pat
  | Exp;

let to_string: t => string;
