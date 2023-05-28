open Sexplib.Std;
open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = int;

let compare = Int.compare;

let min = Int.min_int;

let max = Int.max_int;
let max_op = max - 1;

let lt = (~a=?, l, r) =>
  compare(l, r) < 0 || compare(l, r) == 0 && a == Some(Dir.R);
let gt = (~a=?, l, r) =>
  compare(l, r) > 0 || compare(l, r) == 0 && a == Some(Dir.L);

let is_bounded = (~on: Dir.t, ~a=?, p, bound) =>
  switch (on) {
  | L => lt(~a?, bound, p)
  | R => gt(~a?, p, bound)
  };
