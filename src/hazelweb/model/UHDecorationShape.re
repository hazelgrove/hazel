[@deriving sexp]
type t =
  | ErrHole
  | VarErrHole
  | VarUse
  | CurrentTerm
  | AssertStatus(AssertMap.assert_report);
