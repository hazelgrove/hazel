[@deriving (sexp, show)]
type t =
  | Hole(MetaVar.t)
  | Var(Var.t);
