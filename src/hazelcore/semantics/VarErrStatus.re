module HoleReason = {
  /* Variable: reason */
  [@deriving sexp]
  type t =
    | Free
    | Keyword(IntermediateKeyword.t);
};

/* Variable: var_err */
[@deriving sexp]
type t =
  | NotInVarHole
  | InVarHole(HoleReason.t, MetaVar.t);
