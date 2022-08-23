/** De Bruijn indices */
open Sexplib.Std;

[@deriving sexp]
type s('pos) = int;

let to_string = Int.to_string;

let equal = Int.equal;
let increment = Int.succ;
let decrement = Int.pred;

let shift = (~above: int, ~amount: int, i: s('pos)): s('pos) =>
  i >= above ? i + amount : i;

[@deriving sexp]
type absolute;

[@deriving sexp]
type relative;

module Abs = {
  [@deriving sexp]
  type t = s(absolute);
  let of_int = i => i;
  let to_int = i => i;
  let to_rel = (~offset=0, i) => i + offset;
};

module Rel = {
  [@deriving sexp]
  type t = s(relative);
  let of_int = i => i;
  let to_int = i => i;
  let to_abs = (~offset=0, i) => i + offset;
};
