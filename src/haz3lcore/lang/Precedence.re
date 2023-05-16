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
let power = 3;
let mult = 4; // divide, mod, bitwise-and, bitwise-or, bitwise-xor
let plus = 5; // minus
let cons = 6;
let concat = 7;
let eqs = 8; // less-than, greater-than, inequality
let and_ = 9;
let or_ = 10;
let ann = 11;
let if_ = 12;
let fun_ = 13;
let prod = 14;
let semi = 15;
let let_ = 16;
let rule_arr = 17;
let rule_pre = 18;
let rule_sep = 19;
let case_ = 20;

let min = 21;

let compare = (p1: t, p2: t): int =>
  (-1) * Int.compare((p1 :> int), (p2 :> int));
// let min = (p1: t, p2: t): t => max(p1, p2);

let associativity_map: IntMap.t(Direction.t) =
  [
    (mult, Direction.Left),
    (plus, Left),
    (power, Right),
    (cons, Right),
    (concat, Right),
    (ann, Left),
    (eqs, Left),
  ]
  |> List.to_seq
  |> IntMap.of_seq;

let associativity = (p: t): option(Direction.t) =>
  IntMap.find_opt(p, associativity_map);
