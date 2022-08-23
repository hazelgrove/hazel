/** De Bruijn indices */

[@deriving sexp]
type s('pos) = pri int;

let to_string: s('pos) => string;

let equal: (s('pos), s('pos)) => bool;
let increment: s('pos) => s('pos);
let decrement: s('pos) => s('pos);

let shift: (~above: int, ~amount: int, s('pos)) => s('pos);

[@deriving sexp]
[@sexp.opaque]
type absolute;

[@deriving sexp]
[@sexp.opaque]
type relative;

module Abs: {
  type rel := s(relative);

  [@deriving sexp]
  type t = s(absolute);
  let of_int: int => t;
  let to_int: t => int;
  let to_rel: (~offset: int=?, t) => rel;
};

module Rel: {
  type abs := s(absolute);

  [@deriving sexp]
  type t = s(relative);
  let of_int: int => t;
  let to_int: t => int;
  let to_abs: (~offset: int=?, t) => abs;
};
