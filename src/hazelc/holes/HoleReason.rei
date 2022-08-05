[@deriving (sexp, eq, ord)]
type t =
  | TypeInconsistent
  | WrongLength;
