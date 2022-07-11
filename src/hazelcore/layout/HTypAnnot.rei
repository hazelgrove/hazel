[@deriving sexp]
type t =
  | HoleLabel
  | Delim
  | EmptyTagHole(MetaVar.t)
  | Step(int)
  | Term;
