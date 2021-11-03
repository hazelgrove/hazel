open Sexplib.Std;

[@deriving sexp]
type t =
  | InvalidInput(int)
  | BoxedValue(DHExp.t)
  | Indet(DHExp.t);
