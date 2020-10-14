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
  | ErrHole
  | VarErrHole
  | VarUse
  | CurrentTerm;
