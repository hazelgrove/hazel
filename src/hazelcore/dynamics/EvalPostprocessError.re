[@deriving sexp]
type t =
  | ClosureInsideClosure
  | BoundVarOutsideClosure(Var.t)
  | UnevalOutsideClosure
  | InvalidClosureBody
  | PostprocessedNonHoleInClosure
  | PostprocessedHoleOutsideClosure;

[@deriving sexp]
exception Exception(t);
