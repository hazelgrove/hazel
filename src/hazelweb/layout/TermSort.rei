[@deriving sexp]
type t =
  | Typ
  | Pat
  | Exp
  | Meta;

let string_of: t => string;
