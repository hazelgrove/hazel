[@deriving sexp]
type t =
  | Typ
  | Pat
  | Exp
  | Comment;

let string_of: t => string =
  fun
  | Typ => "typ"
  | Pat => "pat"
  | Exp => "exp"
  | Comment => "comment";
