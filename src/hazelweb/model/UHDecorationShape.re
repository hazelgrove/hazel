[@deriving sexp]
type t =
  | ErrHole
  | VarErrHole
  | VarUse
  | TyVarUse
  | CurrentTerm
  | TestStatus(TestMap.test_report);
