open Sexplib.Std;

[@deriving sexp]
type t = int;

let compare = Int.compare;
