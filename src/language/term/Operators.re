open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type mode =
  | Nat
  | Int
  | SInt
  | Float;

let default_mode = Int;

/* ========== DEFINITIONS ========== */

[@deriving (show({with_path: false}), sexp, yojson, eq, enumerate)]
type op_un_bool =
  | Not;

[@deriving (show({with_path: false}), sexp, yojson, eq, enumerate)]
type op_un_meta =
  | Unquote;

[@deriving (show({with_path: false}), sexp, yojson, eq, enumerate)]
type op_un_num =
  | Minus;

[@deriving (show({with_path: false}), sexp, yojson, eq, enumerate)]
type op_bin_bool =
  | And
  | Or;

[@deriving (show({with_path: false}), sexp, yojson, eq, enumerate)]
type op_bin_num =
  | Plus
  | Minus
  | Times
  | Power
  | Divide
  | LessThan
  | LessThanOrEqual
  | GreaterThan
  | GreaterThanOrEqual
  | Equals
  | NotEquals;

[@deriving (show({with_path: false}), sexp, yojson, eq, enumerate)]
type op_bin_string =
  | Concat
  | Equals;

[@deriving (show({with_path: false}), sexp, yojson, eq, enumerate)]
type op_un =
  | Meta(op_un_meta)
  | Int(op_un_num)
  | Nat(op_un_num)
  | SInt(op_un_num)
  | Float(op_un_num)
  | Bool(op_un_bool);

[@deriving (show({with_path: false}), sexp, yojson, eq, enumerate)]
type op_bin =
  | Int(op_bin_num)
  | SInt(op_bin_num)
  | Nat(op_bin_num)
  | Float(op_bin_num)
  | Bool(op_bin_bool)
  | String(op_bin_string);

[@deriving (show({with_path: false}), sexp, yojson, eq, enumerate)]
type ap_direction =
  | Forward
  | Reverse;

/* ========== ELABORATION ========== */

type bad_literal =
  | BadInt(string);

let replace_literal =
    (lit: Atom.t, ana: option(Atom.cls), use_mode: option(mode))
    : Either.t(Atom.t, bad_literal) => {
  switch (lit, ana, use_mode) {
  | (_, None, None) => L(lit)
  | (Int(n), Some(Int), _) => L(Int(n))
  | (Int(n), Some(Nat), _) => L(Nat(n))
  | (Int(n), Some(Float), _) => L(Float(Bigint.to_float(n)))
  | (Int(n), Some(SInt), _) =>
    switch (Bigint.to_int(n)) {
    | Some(i) => L(SInt(i))
    | None => R(BadInt(Bigint.to_string(n)))
    }
  | (Int(n), _, Some(Int)) => L(Int(n))
  | (Int(n), _, Some(Nat)) => L(Nat(n))
  | (Int(n), _, Some(Float)) => L(Float(Bigint.to_float(n)))
  | (Int(n), _, Some(SInt)) =>
    switch (Bigint.to_int(n)) {
    | Some(i) => L(SInt(i))
    | None => R(BadInt(Bigint.to_string(n)))
    }
  | (Int(n), Some(Bool | String), None) => L(Int(n))
  | (SInt(n), _, _) => L(SInt(n))
  | (Float(n), _, _) => L(Float(n))
  | (Bool(b), _, _) => L(Bool(b))
  | (String(s), _, _) => L(String(s))
  | (Nat(n), _, _) => L(Nat(n))
  };
};

let replace_un_op = (op: op_un, use_mode: option(mode)): op_un => {
  switch (op, use_mode) {
  | (op, None) => op
  | (Int(op) | Nat(op) | Float(op) | SInt(op), Some(Int)) => Int(op)
  | (Int(op) | Nat(op) | Float(op) | SInt(op), Some(SInt)) => SInt(op)
  | (Int(op) | Nat(op) | Float(op) | SInt(op), Some(Nat)) => Nat(op)
  | (Int(op) | Nat(op) | Float(op) | SInt(op), Some(Float)) => Float(op)
  | (Bool(op), _) => Bool(op)
  | (Meta(op), _) => Meta(op)
  };
};

let replace_bin_op = (op: op_bin, use_mode: option(mode)): op_bin => {
  switch (op, use_mode) {
  | (op, None) => op
  | (Int(op), Some(Int)) => Int(op)
  | (Int(op), Some(Nat)) => Nat(op)
  | (Int(op), Some(Float)) => Float(op)
  | (Int(op), Some(SInt)) => SInt(op)
  | (SInt(op), _) => SInt(op)
  | (Nat(op), _) => Nat(op)
  | (Float(op), _) => Float(op)
  | (Bool(op), _) => Bool(op)
  | (String(op), _) => String(op)
  };
};

/* ========== PRINTING ========== */

let show_op_un_meta: op_un_meta => string =
  fun
  | Unquote => "Un-quotation";

let show_op_un_bool: op_un_bool => string =
  fun
  | Not => "Boolean Negation";

let show_op_un_num: op_un_num => string =
  fun
  | Minus => "Integer Negation";

let show_unop: op_un => string =
  fun
  | Meta(op) => show_op_un_meta(op)
  | Bool(op) => show_op_un_bool(op)
  | Float(op)
  | Nat(op)
  | SInt(op)
  | Int(op) => show_op_un_num(op);

let show_op_bin_bool: op_bin_bool => string =
  fun
  | And => "Boolean Conjunction"
  | Or => "Boolean Disjunction";

let show_op_bin_num: op_bin_num => string =
  fun
  | Plus => "Addition"
  | Minus => "Subtraction"
  | Times => "Multiplication"
  | Power => "Exponentiation"
  | Divide => "Division"
  | LessThan => "Less Than"
  | LessThanOrEqual => "Less Than or Equal"
  | GreaterThan => "Greater Than"
  | GreaterThanOrEqual => "Greater Than or Equal"
  | Equals => "Equality"
  | NotEquals => "Inequality";

let show_op_bin_float: op_bin_num => string =
  fun
  | Plus => "Float Addition"
  | Minus => "Float Subtraction"
  | Times => "Float Multiplication"
  | Power => "Float Exponentiation"
  | Divide => "Float Division"
  | LessThan => "Float Less Than"
  | LessThanOrEqual => "Float Less Than or Equal"
  | GreaterThan => "Float Greater Than"
  | GreaterThanOrEqual => "Float Greater Than or Equal"
  | Equals => "Float Equality"
  | NotEquals => "Float Inequality";

let show_op_bin_string: op_bin_string => string =
  fun
  | Concat => "String Concatenation"
  | Equals => "String Equality";

let show_binop: op_bin => string =
  fun
  | Int(op)
  | SInt(op)
  | Nat(op) => show_op_bin_num(op)
  | Float(op) => show_op_bin_float(op)
  | Bool(op) => show_op_bin_bool(op)
  | String(op) => show_op_bin_string(op);

let bool_op_to_string = (op: op_bin_bool): string => {
  switch (op) {
  | And => "&&"
  | Or => "||"
  };
};

let int_op_to_string = (op: op_bin_num): string => {
  switch (op) {
  | Plus => "+"
  | Minus => "-"
  | Times => "*"
  | Power => "**"
  | Divide => "/"
  | LessThan => "<"
  | LessThanOrEqual => "<="
  | GreaterThan => ">"
  | GreaterThanOrEqual => ">="
  | Equals => "=="
  | NotEquals => "!="
  };
};

let float_op_to_string = (op: op_bin_num): string => {
  switch (op) {
  | Plus => "+."
  | Minus => "-."
  | Times => "*."
  | Power => "**."
  | Divide => "/."
  | LessThan => "<."
  | LessThanOrEqual => "<=."
  | GreaterThan => ">."
  | GreaterThanOrEqual => ">=."
  | Equals => "==."
  | NotEquals => "!=."
  };
};

let string_op_to_string = (op: op_bin_string): string => {
  switch (op) {
  | Concat => "++"
  | Equals => "$=="
  };
};

let bin_op_to_string = (op: op_bin): string => {
  switch (op) {
  | SInt(op)
  | Int(op)
  | Nat(op) => int_op_to_string(op)
  | Float(op) => float_op_to_string(op)
  | Bool(op) => bool_op_to_string(op)
  | String(op) => string_op_to_string(op)
  };
};

/* ========== SEMANTICS ========== */

type un_semantics =
  | Defined(
      Atom.kind('a),
      Atom.kind('b),
      'a => Either.t('b, InvalidOperationError.t),
    )
    : un_semantics
  | Undefined(string);

let just = (f, x) => Either.L(f(x));

let semantics_of_un_op = (op: op_un): un_semantics =>
  switch (op) {
  | Int(Minus) => Defined(Int, Int, just(Bigint.neg))
  | Float(Minus) => Defined(Float, Float, just(x => -. x))
  | SInt(Minus) => Defined(SInt, SInt, just(x => - x))
  | Nat(Minus) => Undefined("Cannot negate a natural number")
  | Bool(Not) => Defined(Bool, Bool, just(x => !x))
  | Meta(Unquote) => failwith("semantics of Meta Unquote") // should be unreachable
  };

type bin_semantics =
  | Defined(
      Atom.kind('a),
      Atom.kind('b),
      Atom.kind('c),
      ('a, 'b) => Either.t('c, InvalidOperationError.t),
    )
    : bin_semantics
  | Undefined(string);

let just = (f, x, y) => Either.L(f(x, y));
let int_power = (x, y) =>
  if (y < Bigint.zero) {
    Either.R(InvalidOperationError.NegativeExponent);
  } else {
    Either.L(Bigint.pow(x, y));
  };
let sint_power = (x, y) =>
  if (y < 0) {
    Either.R(InvalidOperationError.NegativeExponent);
  } else {
    Either.L(IntUtil.ipow(x, y));
  };
let int_divide = (x, y) =>
  if (Bigint.equal(y, Bigint.zero)) {
    Either.R(InvalidOperationError.DivideByZero);
  } else {
    Either.L(Bigint.(/)(x, y));
  };
let sint_divide = (x, y) =>
  if (y == 0) {
    Either.R(InvalidOperationError.DivideByZero);
  } else {
    Either.L(x / y);
  };

let semantics_of_bin_op = (op: op_bin): bin_semantics =>
  switch (op) {
  | Int(Plus) => Defined(Int, Int, Int, just(Bigint.(+)))
  | Int(Minus) => Defined(Int, Int, Int, just(Bigint.(-)))
  | Int(Times) => Defined(Int, Int, Int, just(Bigint.( * )))
  | Int(Power) => Defined(Int, Int, Int, int_power)
  | Int(Divide) => Defined(Int, Int, Int, int_divide)
  | Int(LessThan) => Defined(Int, Int, Bool, just((<)))
  | Int(LessThanOrEqual) => Defined(Int, Int, Bool, just((<=)))
  | Int(GreaterThan) => Defined(Int, Int, Bool, just((>)))
  | Int(GreaterThanOrEqual) => Defined(Int, Int, Bool, just((>=)))
  | Int(Equals) => Defined(Int, Int, Bool, just((==)))
  | Int(NotEquals) => Defined(Int, Int, Bool, just((!=)))

  | SInt(Plus) => Defined(SInt, SInt, SInt, just((+)))
  | SInt(Minus) => Defined(SInt, SInt, SInt, just((-)))
  | SInt(Times) => Defined(SInt, SInt, SInt, just(( * )))
  | SInt(Power) => Defined(SInt, SInt, SInt, sint_power)
  | SInt(Divide) => Defined(SInt, SInt, SInt, sint_divide)
  | SInt(LessThan) => Defined(SInt, SInt, Bool, just((<)))
  | SInt(LessThanOrEqual) => Defined(SInt, SInt, Bool, just((<=)))
  | SInt(GreaterThan) => Defined(SInt, SInt, Bool, just((>)))
  | SInt(GreaterThanOrEqual) => Defined(SInt, SInt, Bool, just((>=)))
  | SInt(Equals) => Defined(SInt, SInt, Bool, just((==)))
  | SInt(NotEquals) => Defined(SInt, SInt, Bool, just((!=)))

  | Nat(Plus) => Defined(Nat, Nat, Nat, just(Bigint.(+)))
  | Nat(Minus) =>
    Undefined(
      "Cannot subtract natural numbers, for truncated subtraction use `monus`",
    )
  | Nat(Times) => Defined(Nat, Nat, Nat, just(Bigint.( * )))
  | Nat(Power) => Defined(Nat, Nat, Nat, just(Bigint.pow))
  | Nat(Divide) => Defined(Nat, Nat, Nat, int_divide)
  | Nat(LessThan) => Defined(Nat, Nat, Bool, just((<)))
  | Nat(LessThanOrEqual) => Defined(Nat, Nat, Bool, just((<=)))
  | Nat(GreaterThan) => Defined(Nat, Nat, Bool, just((>)))
  | Nat(GreaterThanOrEqual) => Defined(Nat, Nat, Bool, just((>=)))
  | Nat(Equals) => Defined(Nat, Nat, Bool, just((==)))
  | Nat(NotEquals) => Defined(Nat, Nat, Bool, just((!=)))

  | Float(Plus) => Defined(Float, Float, Float, just((+.)))
  | Float(Minus) => Defined(Float, Float, Float, just((-.)))
  | Float(Times) => Defined(Float, Float, Float, just(( *. )))
  | Float(Power) => Defined(Float, Float, Float, just(( ** )))
  | Float(Divide) => Defined(Float, Float, Float, just((/.)))
  | Float(LessThan) => Defined(Float, Float, Bool, just((<)))
  | Float(LessThanOrEqual) => Defined(Float, Float, Bool, just((<=)))
  | Float(GreaterThan) => Defined(Float, Float, Bool, just((>)))
  | Float(GreaterThanOrEqual) => Defined(Float, Float, Bool, just((>=)))
  | Float(Equals) => Defined(Float, Float, Bool, just((==)))
  | Float(NotEquals) => Defined(Float, Float, Bool, just((!=)))

  | String(Concat) => Defined(String, String, String, just((++)))
  | String(Equals) => Defined(String, String, Bool, just((==)))

  | Bool(And) => Defined(Bool, Bool, Bool, just((&&))) // Note: booleans have extra short-cutting rules in transition
  | Bool(Or) => Defined(Bool, Bool, Bool, just((||)))
  };

/* ========== BUILTINS ========== */

let op_name = (op: op_bin): string =>
  switch (op) {
  | Int(Plus) => "int_plus"
  | Int(Minus) => "int_minus"
  | Int(Times) => "int_times"
  | Int(Power) => "int_power"
  | Int(Divide) => "int_divide"
  | Int(LessThan) => "int_lt"
  | Int(LessThanOrEqual) => "int_lte"
  | Int(GreaterThan) => "int_gt"
  | Int(GreaterThanOrEqual) => "int_gte"
  | Int(Equals) => "int_eq"
  | Int(NotEquals) => "int_neq"
  | SInt(Plus) => "sint_plus"
  | SInt(Minus) => "sint_minus"
  | SInt(Times) => "sint_times"
  | SInt(Power) => "sint_power"
  | SInt(Divide) => "sint_divide"
  | SInt(LessThan) => "sint_lt"
  | SInt(LessThanOrEqual) => "sint_lte"
  | SInt(GreaterThan) => "sint_gt"
  | SInt(GreaterThanOrEqual) => "sint_gte"
  | SInt(Equals) => "sint_eq"
  | SInt(NotEquals) => "sint_neq"
  | Nat(Plus) => "nat_plus"
  | Nat(Minus) => "nat_minus"
  | Nat(Times) => "nat_times"
  | Nat(Power) => "nat_power"
  | Nat(Divide) => "nat_divide"
  | Nat(LessThan) => "nat_lt"
  | Nat(LessThanOrEqual) => "nat_lte"
  | Nat(GreaterThan) => "nat_gt"
  | Nat(GreaterThanOrEqual) => "nat_gte"
  | Nat(Equals) => "nat_eq"
  | Nat(NotEquals) => "nat_neq"
  | Float(Plus) => "float_plus"
  | Float(Minus) => "float_minus"
  | Float(Times) => "float_times"
  | Float(Power) => "float_power"
  | Float(Divide) => "float_divide"
  | Float(LessThan) => "float_lt"
  | Float(LessThanOrEqual) => "float_lte"
  | Float(GreaterThan) => "float_gt"
  | Float(GreaterThanOrEqual) => "float_gte"
  | Float(Equals) => "float_eq"
  | Float(NotEquals) => "float_neq"
  | String(Concat) => "string_concat"
  | String(Equals) => "string_eq"
  | Bool(And) => "bool_and"
  | Bool(Or) => "bool_or"
  };

let builtins = {
  Atom.(
    all_of_op_bin
    |> List.filter_map(op =>
         switch (semantics_of_bin_op(op)) {
         | Undefined(_) => None
         | Defined(x, y, z, f) => Some((op_name(op), TwoFun(x, y, z, f)))
         }
       )
  );
};
