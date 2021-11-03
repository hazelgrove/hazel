open Sexplib.Std;

[@deriving sexp]
type t =
  | ErrHole
  | VarErrHole
  | VarUse
  | CurrentTerm
  | AssertStatus(list(AssertStatus.t));
