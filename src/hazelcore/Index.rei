/** De Bruijn indices */

[@deriving sexp]
type t('pos) = pri int;

let to_string: t('pos) => string;

let equal: (t('pos), t('pos)) => bool;
let increment: t('pos) => t('pos);
let decrement: t('pos) => t('pos);

let shift: (~above: int, ~amount: int, t('idx)) => t('idx);

[@deriving sexp]
[@sexp.opaque]
type absolute;

[@deriving sexp]
[@sexp.opaque]
type relative;

module Abs: {
  type rel := t(relative);

  [@deriving sexp]
  type nonrec t = t(absolute);
  let of_int: int => t;
  let to_int: t => int;
  let to_rel: (~offset: int=?, t) => rel;
};

module Rel: {
  type abs := t(absolute);

  [@deriving sexp]
  type nonrec t = t(relative);
  let of_int: int => t;
  let to_int: t => int;
  let to_abs: (~offset: int=?, t) => abs;
};
