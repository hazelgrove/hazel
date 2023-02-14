open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = int;

let compare = Int.compare;

let min = Int.min_int;

let max = Int.max_int;
let max_op = max - 1;

let lt = (~a, l, r) =>
  compare(l, r) < 0 || compare(l, r) == 0 && a(l) == Some(Dir.L);
