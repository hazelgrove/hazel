[@deriving sexp]
type t =
  | TypeInconsistent
  | WrongLength;

let equal = (x, y) => x == y;
