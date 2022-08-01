open Sexplib.Std;

[@deriving sexp]
type t = int;

let equal = (x: t, y: t) => x === y;
