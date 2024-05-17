open Sexplib.Std;
open Util;

/**
 * higher precedence means lower int representation
 */
[@deriving (show({with_path: false}), sexp, yojson)]
type t = int;

let max: t = 0;

let unquote = 1;
let ap = 2;
let neg = 3;
let power = 4;
let mult = 5;
let plus = 6;
let cons = 7;
let concat = 8;
let eqs = 9;
let and_ = 10;
let or_ = 11;
let ann = 12;
let if_ = 13;
let fun_ = 14;
let prop_and = 15;
let prop_or = 16;
let prop_implies = 17;
let judgement_entail = 18;
let prod = 19;
let semi = 20;
let let_ = 21;
let filter = 22;
let rule_arr = 23;
let rule_pre = 24;
let rule_sep = 25;
let case_ = 26;
let min = 27;

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
