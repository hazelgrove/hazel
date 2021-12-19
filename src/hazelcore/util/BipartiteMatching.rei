include (module type of IntMap);

[@deriving sexp]
type v =
  | Unmatched
  | Matched(int);

[@deriving sexp]
type nonrec t = t(v);

[@deriving sexp]
type binding = (int, v);

let match: (int, int, t) => t;

let unmatch: (int, t) => t;

let init: IntSet.t => t;

let unmatched_sources: t => IntSet.t;

let matched_sources: t => IntSet.t;

let matched_targets: t => IntSet.t;

let unmatched_targets: (IntSet.t, t) => IntSet.t;

let find_sources: (int, t) => IntSet.t;

let is_perfect: t => bool;
