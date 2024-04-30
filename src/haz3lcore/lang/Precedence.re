open Sexplib.Std;
open Util;

/**
 * higher precedence means lower int representation
 *
 * These precedences are interspersed with examples to help you
 * work out the precedence. For each example, if a construct
 * requires parentheses when placed in the '_____' space, then
 * your new construct's precedence is below the comment with
 * the example. (i.e. higher int)
 */
[@deriving (show({with_path: false}), sexp, yojson)]
type t = int;

let max: t = 0;

let unquote = 1;
let cast = 2;
let ap = 3;
// _____(x)
let neg = 4;
// _____ ** 2
let power = 5;
// 2 ** _____
// 6 / _____
let mult = 6;
let not_ = 6;
// _____ / 6
// 4 - _____
let plus = 7;
// _____ - 4
// _____ :: []
let cons = 8;
// 1 :: _____
// [1,2] @ _____
let concat = 9;
// _____ @ [1,2]
// x == _____
let eqs = 10;
// _____ == x
// _____ && true
let and_ = 11;
// true && _____
// _____ || false
let or_ = 12;
// false || _____
let ann = 13;
let if_ = 14;
let fun_ = 15;
// fun x -> _____
let prod = 16;
// a , _____ , x
// _____ ; ()
let semi = 17;
// () ; _____
let let_ = 18;
let rule_arr = 19;
let rule_pre = 20;
let rule_sep = 21;
let case_ = 22;
let min = 24;

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

let of_bin_op: Operators.op_bin => t =
  fun
  | Int(op) =>
    switch (op) {
    | Plus => plus
    | Minus => plus
    | Times => mult
    | Power => power
    | Divide => mult
    | LessThan => eqs
    | LessThanOrEqual => eqs
    | GreaterThan => eqs
    | GreaterThanOrEqual => eqs
    | Equals => eqs
    | NotEquals => eqs
    }
  | Float(op) =>
    switch (op) {
    | Plus => plus
    | Minus => plus
    | Times => mult
    | Power => power
    | Divide => mult
    | LessThan => eqs
    | LessThanOrEqual => eqs
    | GreaterThan => eqs
    | GreaterThanOrEqual => eqs
    | Equals => eqs
    | NotEquals => eqs
    }
  | Bool(op) =>
    switch (op) {
    | And => and_
    | Or => or_
    }
  | String(op) =>
    switch (op) {
    | Concat => concat
    | Equals => eqs
    };
