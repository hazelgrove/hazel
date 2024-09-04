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

let associativity_map: ref(list((t, Direction.t))) = ref([]);
let left_associative = (level: t) => {
  associativity_map := [(level, Direction.Left), ...associativity_map^];
  level;
};
let right_associative = (level: t) => {
  associativity_map := [(level, Direction.Right), ...associativity_map^];
  level;
};

let max: t = 0;

// ========== TYPES ==========
let type_sum_ap = 11;
// _____ (Int)
// + T1 + _____
let type_plus = 12 |> right_associative;
// _____ -> Int
let type_arrow = 13 |> right_associative;
// Int -> _____
// String , _____ , String
let type_prod = 14;
let type_binder = 15;
// forall t -> _____
// rec t -> _____

// ======== PATTERNS =========
// ======= EXPRESSIONS =======

let unquote = 21;
// $_____
let ap = 22;
// _____(x)
// 5 : _____
let cast = 23 |> left_associative;
// _____ : T
// - _____
let neg = 24;
// _____ ** 2
let power = 25 |> right_associative;
// 2 ** _____
// 6 / _____
let mult = 26 |> left_associative;
let not_ = 26;
// _____ / 6
// 4 - _____
let plus = 27 |> left_associative;
// _____ - 4
// _____ :: []
let cons = 28 |> right_associative;
// 1 :: _____
// [1,2] @ _____
let concat = 29 |> right_associative;
// _____ @ [1,2]
// x == _____
let eqs = 30 |> left_associative;
// _____ == x
// _____ && true
let and_ = 31;
// true && _____
// _____ || false
let or_ = 32;
// false || _____
let if_ = 34;
let fun_ = 35;
// fun x -> _____
let prod = 36;
// a , _____ , x
// _____ ; ()
let semi = 37 |> right_associative;
// () ; _____
let let_ = 38;
// let x = 3 in _____
let rule_arr = 39;
let rule_pre = 40;
let rule_sep = 41;
let case_ = 42;

let comma = 45;

let min = 46;

let compare = (p1: t, p2: t): int =>
  (-1) * Int.compare((p1 :> int), (p2 :> int));
// let min = (p1: t, p2: t): t => max(p1, p2);

let associativity_map: IntMap.t(Direction.t) =
  associativity_map^ |> List.to_seq |> IntMap.of_seq;

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
