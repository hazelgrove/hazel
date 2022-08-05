open Sexplib.Std;

[@deriving (sexp, eq, ord)]
type t = int;

let of_int = l => l;
let to_int = l => l;

let init = 0;
let next = l => l + 1;

let max = Int.max;
