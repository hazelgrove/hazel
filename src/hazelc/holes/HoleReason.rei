[@deriving sexp]
type t =
  | TypeInconsistent
  | WrongLength;

let equal: (t, t) => bool;
