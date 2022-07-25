open Sexplib.Std;
open Util;

/**
 * higher precedence means lower int representation
 */
[@deriving (show({with_path: false}), sexp, yojson)]
type t = int;

let max: t = 0;

let fact = 1;
let ap = 2;
let mult = 3;
let plus = 4;
let concat = 5;
let eqs = 6;
let cond = 7;
let prod = 8;
let if_ = 9;
let semi = 10;
let let_ = 11;

let min = 12;

let compare = (p1: t, p2: t): int =>
  (-1) * Int.compare((p1 :> int), (p2 :> int));
// let min = (p1: t, p2: t): t => max(p1, p2);

let associativity_map: IntMap.t(Direction.t) =
  [
    (mult, Direction.Left),
    (plus, Left),
    (concat, Right),
    (prod, Right),
    (cond, Left),
    (eqs, Left),
    (prod, Right),
  ]
  |> List.to_seq
  |> IntMap.of_seq;

let associativity = (p: t): option(Direction.t) =>
  IntMap.find_opt(p, associativity_map);
