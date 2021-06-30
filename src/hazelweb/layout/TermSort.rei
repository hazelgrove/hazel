[@deriving sexp]
type t =
  | Typ
  | Pat
  | Exp
  | Comment;

let string_of: t => string;
