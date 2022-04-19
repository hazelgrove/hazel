/** De Bruijn indices */

[@deriving sexp]
type t('i);

[@deriving sexp]
[@sexp.opaque]
type abs;

[@deriving sexp]
[@sexp.opaque]
type rel;

let abs_to_rel: (~offset: int=?, t(abs)) => t(rel);
let rel_to_abs: (~offset: t(abs)=?, t(rel)) => t(abs);

let of_int: int => t('i);
let to_int: t('i) => int;

let to_string: t('i) => string;

let equal: (t('i), t('i)) => bool;
let increment: t('i) => t('i);
let decrement: t('i) => t('i);
