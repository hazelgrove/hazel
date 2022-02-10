/** De Bruijn indices */

[@deriving sexp]
type t = int;

let to_int: t => int;
let equal: (t, t) => bool;
let increment: t => t;
