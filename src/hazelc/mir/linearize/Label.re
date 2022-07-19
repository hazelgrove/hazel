open Sexplib.Std;

[@deriving sexp]
type t = int;

let init = 0;

let next = l => l + 1;
