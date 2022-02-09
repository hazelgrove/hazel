[@deriving sexp]
type t =
  | StandardErrStatus(ErrStatus.t)
  | InconsistentBranches(list(HTyp.t), MetaVar.t)
  | NotExhaustive;
// TODO: add redundent and exhaustiveness here
