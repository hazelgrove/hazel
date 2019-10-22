[@deriving sexp]
type t =
  | Hole(MetaVar.t)
  | Var(Var.t);
