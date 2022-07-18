open Sexplib.Std;

[@deriving sexp]
type t = int;

let init = 0;
let equal = (==);

let next = id => id + 1;

let invalid = (-1);
let is_invalid = equal(invalid);
