open Sexplib.Std;

[@deriving sexp]
type t =
  | InvalidShape
  | Inline(int)
  | MultiLine(int);
