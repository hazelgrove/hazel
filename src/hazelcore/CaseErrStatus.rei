[@deriving sexp]
type mode =
  | Syn
  | Ana;

[@deriving sexp]
type t =
  | StandardErrStatus(ErrStatus.t)
  | InconsistentBranches(list(HTyp.t), MetaVar.t, mode);
