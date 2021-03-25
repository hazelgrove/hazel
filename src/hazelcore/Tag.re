open Sexplib.Std;

[@deriving sexp]
type t =
  | Tag(string)
  | TagHole;

let compare = compare;
