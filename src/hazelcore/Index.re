/** De Bruijn indices */
// open Sexplib.Std;

// [@deriving sexp]
type t = int;

let to_int: t => int = i => i;

let equal: (t, t) => bool = (==);
let increment: t => t = Int.succ;
