open Sexplib.Std;

[@deriving sexp]
type t =
  | Inline(int)
  | MultiLine(int);
