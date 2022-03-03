[@deriving sexp]
type t =
  | Redundant(MetaVar.t)
  | IndeterminatelyRedundant
  | NotRedundant;
