[@deriving sexp]
type t =
  | Redundant(MetaVar.t)
  | NotRedundant;
