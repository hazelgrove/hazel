module MatchReason = {
  /* Variable: `reason` */
  [@deriving sexp]
  type t =
    | InconsistentBranches
    | NotExhaustive;
};

[@deriving sexp]
type t =
  | MatchErrHole(MatchReason.t)
  | RuleErrHole
  | ErrHole
  | VarErrHole
  | VarUse
  | CurrentTerm;
