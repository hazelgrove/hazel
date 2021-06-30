[@deriving sexp]
type t =
  | Typ
  | Pat
  | Exp
  | Meta;

let string_of: t => string =
  fun
  | Typ => "Typ"
  | Pat => "Pat"
  | Exp => "Exp"
  | Meta => "Meta";
