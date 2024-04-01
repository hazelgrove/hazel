open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = int;

let compare = Int.compare;

let min = Int.min_int;

let max = Int.max_int;
let max_op = max - 1;

let lt = (~a=None, l: Bound.t(t), r) =>
  switch (l) {
  | Root => true
  | Node(l) => compare(l, r) < 0 || compare(l, r) == 0 && a == Some(Dir.R)
  };
let gt = (~a=None, l, r: Bound.t(t)) =>
  switch (r) {
  | Root => true
  | Node(r) => compare(l, r) > 0 || compare(l, r) == 0 && a == Some(Dir.L)
  };
let eq = (~a=None, l, r) => compare(l, r) == 0 && Option.is_none(a);

// let leq = (~a=None, l: Bound.t(t), r) =>
//   switch (l) {
//   | Root => true
//   | Node(l) => lt(~a, l, r) || eq(~a, l, r)
//   };
// let geq = (~a=None, l, r: Bound.t(t)) =>
//   gt(~a, l, r) || eq(~a, l, r);
// let lower_bounded = (~a=None, ~side: Dir.t, p, bound) =>
//   switch (side) {
//   | L => leq(~a?, bound, p)
//   | R => geq(~a?, p, bound)
//   };

module Table = {
  type t('v) = list((option(Dir.t), 'v));
  let singleton = (~a=?, v) => [(a, v)];
  // let map = f => List.mapi((p, (a, v)) => f(p, a, v));
  let map = f => List.map(((a, v)) => (a, f(v)));
  let mapi = f => List.mapi((p, (a, v)) => f((p, a), v));
  let filter = f => List.filteri((p, (a, v)) => f(p, a, v));
};
