[@deriving sexp]
type t =
  | Typ
  | Pat
  | Exp;

let to_string: t => string =
  fun
  | Typ => "typ"
  | Exp => "exp"
  | Pat => "pat";
