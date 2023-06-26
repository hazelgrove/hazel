open Sexplib.Std;
open Util;

/**
 * higher precedence means lower int representation
 */
[@deriving (show({with_path: false}), sexp, yojson)]
type t = int;

let max: t = 0;

let ap = 1; // function application and constructor application
let neg = 2; // unary negation
let power = 3; // power operator
let mult = 4; // times, divide, bitwise-and, bitwise-or operators
let plus = 5; // plus, minus operators
let cons = 6; // cons operator
let concat = 7; // list concatenation. Currently not in use.
let eqs = 8; // equals, less than, greater than operators
let and_ = 9; // logical-and operator
let or_ = 10; // logical-or operator
let ann = 11; // type annotation construction
let if_ = 12; // if construction
let semi = 13; // sequence construction
let fun_ = 14; // function construction
let prod = 15; // commas for tuples, types, and in let expressions
let let_ = 16; // let construction
let rule_arr = 17; // unused
let rule_pre = 18; // unused
let rule_sep = 19; // case rule | =>
let case_ = 20; // unused

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
