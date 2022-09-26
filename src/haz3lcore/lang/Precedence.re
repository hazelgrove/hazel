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
let and_ = 8;
let or_ = 9;
let ann = 10;
let prod = 11;
let if_ = 12;
let semi = 13;
let let_ = 14;

let rule_arr = 15;
let rule_pre = 16;
let rule_sep = 17;
let case_ = 18;

let min = 19;

let compare = (p1: t, p2: t): int =>
  (-1) * Int.compare((p1 :> int), (p2 :> int));
// let min = (p1: t, p2: t): t => max(p1, p2);

let associativity_map: IntMap.t(Direction.t) =
  [
    (mult, Direction.Left),
    (plus, Left),
    (cons, Right),
    (concat, Right),
    (ann, Left),
    (eqs, Left),
  ]
  |> List.to_seq
  |> IntMap.of_seq;

let associativity = (p: t): option(Direction.t) =>
  IntMap.find_opt(p, associativity_map);
