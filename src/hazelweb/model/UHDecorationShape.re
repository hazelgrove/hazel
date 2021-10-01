open Sexplib.Std;

[@deriving sexp]
type t =
  | ErrHole
  | VarErrHole
  | VarUse
  | CurrentTerm
  | ExplanationElems(string);
