open Sexplib.Std;

[@deriving sexp]
type t = int;
let eq = (x: t, y: t) => x === y;
let compare = Int.compare;
let join: (t, t) => t = min;
