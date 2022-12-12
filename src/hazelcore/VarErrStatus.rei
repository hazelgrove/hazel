module HoleReason: {
  /* Variable: reason */
  [@deriving sexp]
  type t =
    | Free
    | ExpandingKeyword(ExpandingKeyword.t) /* Variable: var_err */;
};

[@deriving sexp]
type t =
  | NotInVarHole
  | InVarHole(HoleReason.t, MetaVar.t);
