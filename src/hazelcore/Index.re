/** De Bruijn indices */
open Sexplib.Std;

[@deriving sexp]
type t('i) = int;

/* Absolute Indices */

[@deriving sexp]
[@sexp.opaque]
type abs;

/* Relative Indices */

[@deriving sexp]
[@sexp.opaque]
type rel;

let abs_to_rel = (~offset: int=0, i: t(abs)): t(rel) => i - offset;
let rel_to_abs = (~offset: t(abs)=0, i: t(rel)): t(abs) => i + offset;

let of_int = i => i;
let to_int = i => i;

let to_string = Int.to_string;

// polymorphic index functions
let equal = Int.equal;
let increment = Int.succ;
let decrement = Int.pred;
