open Sexplib.Std;

[@deriving sexp]
type t =
  | Tag(string)
  | TagHole;
