[@deriving sexp]
type t =
  | Int
  | Float
  | Bool
  | ExpandingKeyword(ExpandingKeyword.t)
  | TyVar(TyVar.t)
  | InvalidText(string);

let builtin: string => option(t);
let of_string: string => t;
