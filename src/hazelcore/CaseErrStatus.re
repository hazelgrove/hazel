[@deriving sexp]
type mode =
  | Syn
  | Ana;

[@deriving sexp]
type t =
  | CaseNotInHole
  | InconsistentBranches(MetaVar.t, mode);
