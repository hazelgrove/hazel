open Sexplib.Std;
open Util;

/**
 * higher precedence means lower int representation
 */
[@deriving (show({with_path: false}), sexp, yojson)]
type t = int;

let max: t = 0;

let ap = 1;
let neg = 2;
let mult = 3;
let plus = 4;
let cons = 5;
let concat = 6;
let eqs = 7;
let ann = 8;
let prod = 9;
let if_ = 10;
let semi = 11;
let let_ = 12;

let rule_arr = 13;
let rule_pre = 14;
let rule_sep = 15;
let case_ = 16;

let min = 16;

let compare = (p1: t, p2: t): int =>
  (-1) * Int.compare((p1 :> int), (p2 :> int));
// let min = (p1: t, p2: t): t => max(p1, p2);

let associativity_map: IntMap.t(Direction.t) =
  [
    (mult, Direction.Left),
    (plus, Left),
    (cons, Right),
    (concat, Right),
    (prod, Right),
    (ann, Left),
    (eqs, Left),
    (rule_arr, Right),
    (rule_sep, Right),
  ]
  |> List.to_seq
  |> IntMap.of_seq;

let associativity = (p: t): option(Direction.t) =>
  IntMap.find_opt(p, associativity_map);
