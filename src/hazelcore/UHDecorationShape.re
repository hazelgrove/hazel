[@deriving sexp]
type t =
  | ErrHole
  | VarErrHole
  | VarUse
  | CurrentTerm
  | TestStatus(TestMap.test_report);
