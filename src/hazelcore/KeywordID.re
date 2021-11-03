open Sexplib.Std;

[@deriving sexp]
type t = int;
let eq = (x: t, y: t) => x === y;

let init: t = 0;
