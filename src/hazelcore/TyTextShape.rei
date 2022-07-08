[@deriving sexp]
type t =
  | Int
  | Float
  | Bool
  | ExpandingKeyword(ExpandingKeyword.t)
  | TyVar(string);

let builtin: string => option(t);
let of_string: string => option(t);
