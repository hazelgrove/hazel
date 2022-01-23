/** De Bruijn indices */
open Sexplib.Std;

[@deriving sexp]
type t = int;

let equal: (t, t) => bool = (==);
let increment: t => t = Int.succ;
