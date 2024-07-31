open Util;

/**
 * higher precedence means lower int representation
 */
[@deriving (show({with_path: false}), sexp, yojson)]
type t = int;

let max: t = 0;

let unquote = 1;
let dot = 2;
let ap = 3;
let neg = 4;
let power = 5;
let mult = 6;
let plus = 7;
let cons = 8;
let concat = 9;
let eqs = 10;
let and_ = 11;
let or_ = 12;
let ann = 13;
let if_ = 14;
let fun_ = 15;
let lab = 16;
let semi = 18;
let let_ = 19;
let filter = 20;
let rule_arr = 21;
let rule_pre = 22;
let rule_sep = 23;
let case_ = 24;

let comma = 17;

let type_plus = 5;
let type_arrow = 6;
let type_prod = comma;

let min = 26;

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
    (dot, Left),
    (type_arrow, Right),
  ]
  |> List.to_seq
  |> IntMap.of_seq;

let associativity = (p: t): option(Direction.t) =>
  IntMap.find_opt(p, associativity_map);
