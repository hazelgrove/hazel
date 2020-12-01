open Sexplib.Std;

[@deriving sexp]
type t =
  | ErrHole
  | VarErrHole
  | VarUse
  | CurrentTerm
  | AssertResult(list(AssertResult.t));
