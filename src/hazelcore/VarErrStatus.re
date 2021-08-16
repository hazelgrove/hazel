module HoleReason = {
  /* Variable: reason */
  // TODO rename Keyword to ExpandingKeyword
  [@deriving (sexp, show)]
  type t =
    | Free
    | Keyword(ExpandingKeyword.t);
};

/* Variable: var_err */
[@deriving (sexp, show)]
type t =
  | NotInVarHole
  | InVarHole(HoleReason.t, MetaVar.t);
