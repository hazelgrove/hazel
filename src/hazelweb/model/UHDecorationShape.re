module CaseReason = {
  /* Variable: `reason` */
  [@deriving sexp]
  type t =
    | InconsistentBranches
    | NotExhaustive;
};

[@deriving sexp]
type t =
  | CaseErrHole(CaseReason.t)
  | RuleErrHole
  | ErrHole
  | VarErrHole
  | VarUse
  | TyVarUse
  | CurrentTerm;
