open Sexplib.Std;

[@deriving sexp]
type t =
  | ErrHole(CursorPath.steps)
  | VarErrHole(CursorPath.steps)
  | VarUse(CursorPath.steps)
  | CurrentTerm(CursorPath.t)
  | ExplanationElems((CursorPath.steps, string));
