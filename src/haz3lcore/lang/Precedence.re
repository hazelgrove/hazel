open Sexplib.Std;
open Util;

/**
 * higher precedence means higher int representation
 */
[@deriving (show({with_path: false}), sexp, yojson)]
type t = int;

let max: t = 22;

let ap = 21; // function application and constructor application
let neg = 20; // unary negation and not operator
let power = 19; // power operator
let mult = 18; // times, divide, bitwise-and, bitwise-or
let plus = 17; // plus, minus, string_concat, list_concat
let arrow = 16; // arrow type
let cons = 15; // cons operator
let concat = 14; // unused
let eqs = 13; // equals, less-than, greater-than
let and_ = 12; // logical-and
let or_ = 11; // logical-or, type_plus, type_sum
let ann = 10; // type annotation construction
let if_ = 9; // if construction
let semi = 8; // sequence construction
let fun_ = 7; // function construction
let prod = 6; // commas for tuples
let let_ = 5; // let construction
let rule_arr = 4; // unused
let rule_pre = 3; // unused
let rule_sep = 2; // case rule | =>
let case_ = 1; // unused

let min = 0;

let compare = (p1: t, p2: t): int => Int.compare((p1 :> int), (p2 :> int));
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
