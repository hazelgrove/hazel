open Sexplib.Std;

[@deriving (sexp, show)]
type t = int;
let eq = (x: t, y: t) => x === y;
