type t('a) = list('a);

/* Construction */

let none = [];

let from_list = xs => xs;

/* Collection */

let to_list = xs => xs;

/* Core functions */

let map = List.map;

let pure = List2.pure;

let join = List.concat;

/* Generic library functions */

let pure_bind = (xs, f) => map(f, xs);

let bind = (xs, f) => map(f, xs) |> join;

let and_then = (f, xs) => bind(xs, f);

let guard = b =>
  if (b) {
    pure();
  } else {
    none;
  };

/* Specific library functions */

let union = List.concat;

let one_of_each = List2.sequence;

let is_empty = xs => xs == none;

let filter = List.filter;

let dedup = xs => List.sort_uniq(compare, xs);

let collapse_option = List2.filter_somes;

let take = List2.take;

let curb_overflow = (n, xs) =>
  if (List.compare_length_with(xs, n) > 0) {
    none;
  } else {
    xs;
  };

/* Lifting */

let lift_option = op => collapse_option([op]);

let lift_result = r => Result2.to_option(r) |> lift_option;

/* Syntax */

module Syntax = {
  let (let+) = pure_bind;
  let ( let* ) = bind;
};
