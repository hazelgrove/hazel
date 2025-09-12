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
[@deriving (show({with_path: false}), sexp, yojson, eq)]
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
// poly t -> _____
// rec t -> _____

// ======== PATTERNS =========
// ======= EXPRESSIONS =======

let unquote = 21;
// $_____
let dot = 22 |> left_associative;
let ap = 23;
// _____(x)
// 5 : _____
let asc = 24 |> left_associative;
// _____ : T
// - _____
let neg = 25;
// _____ ** 2
let power = 26 |> right_associative;
// 2 ** _____
// 6 / _____
let mult = 27 |> left_associative;
let not_ = 27;
// _____ / 6
// 4 - _____
let plus = 28 |> left_associative;
// _____ - 4
// _____ :: []
let cons = 29 |> right_associative;
// 1 :: _____
// [1,2] @ _____
let concat = 30 |> right_associative;
// _____ @ [1,2]
// x == _____
let eqs = 31 |> left_associative;
// _____ == x
// _____ && true
let and_ = 32 |> right_associative;
// true && _____
// _____ || false
let or_ = 33 |> right_associative;

// Derivation Mode Only
// A /\ B \/ C ==> D
// ((A /\ B) \/ C) ==> D
let impl = 34;
// false || _____
let if_ = 35;
let fun_ = 36;
// fun x -> _____
let prod = 37;
// a , _____ , x
// _____ ; ()
let semi = 38 |> right_associative;
// () ; _____

let lab = 39;

let let_ = 40;
// let x = 3 in _____
let rule_arr = 41;
let rule_pre = 42;
let rule_sep = 43;
let case_ = 44;

let comma = 47;

// Derivation Mode Only
// Exp : Typ (HasType)
// Exp => Typ (Synthesis)
// Exp <= Typ (Analysis)
let ann = 48;

let min = 49;

let compare = (p1: t, p2: t): int =>
  (-1) * Int.compare((p1 :> int), (p2 :> int));
// let min = (p1: t, p2: t): t => max(p1, p2);

let associativity_map: IntMap.t(Direction.t) =
  associativity_map^ |> List.to_seq |> IntMap.of_seq;

let associativity = (p: t): option(Direction.t) =>
  IntMap.find_opt(p, associativity_map);

let of_bin_op: Language.Operators.op_bin => t =
  fun
  | Nat(op)
  | SInt(op)
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
    }
  | Poly(op) =>
    switch (op) {
    | Equals => eqs
    | NotEquals => eqs
    };
