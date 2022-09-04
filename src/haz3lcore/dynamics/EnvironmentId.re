open Sexplib.Std;

[@deriving sexp]
type t = int;

let init = 0;
let equal = (==);

let invalid = (-1);
let is_invalid = equal(invalid);
