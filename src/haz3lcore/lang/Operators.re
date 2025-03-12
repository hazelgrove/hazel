open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type mode =
  | Nat
  | Int;

let default_mode = Int;

/* ========== DEFINITIONS ========== */

[@deriving (show({with_path: false}), sexp, yojson, eq, enumerate)]
type op_un_bool =
  | Not;

[@deriving (show({with_path: false}), sexp, yojson, eq, enumerate)]
type op_un_meta =
  | Unquote;

[@deriving (show({with_path: false}), sexp, yojson, eq, enumerate)]
type op_un_int =
  | Minus;

[@deriving (show({with_path: false}), sexp, yojson, eq, enumerate)]
type op_bin_bool =
  | And
  | Or;

[@deriving (show({with_path: false}), sexp, yojson, eq, enumerate)]
type op_bin_int =
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
type op_bin_float =
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
  | Int(op_un_int)
  | Nat(op_un_int)
  | Bool(op_un_bool);

[@deriving (show({with_path: false}), sexp, yojson, eq, enumerate)]
type op_bin =
  | Int(op_bin_int)
  | Nat(op_bin_int)
  | Float(op_bin_float)
  | Bool(op_bin_bool)
  | String(op_bin_string);

[@deriving (show({with_path: false}), sexp, yojson, eq, enumerate)]
type ap_direction =
  | Forward
  | Reverse;

/* ========== ELABORATION ========== */

let replace_literal = (lit: Atom.t, use_mode: option(mode)): Atom.t => {
  switch (lit, use_mode) {
  | (_, None) => lit
  | (Int(n) | Nat(n), Some(Int)) => Int(n)
  | (Int(n) | Nat(n), Some(Nat)) => Nat(n)
  | (Float(n), _) => Float(n)
  | (Bool(b), _) => Bool(b)
  | (String(s), _) => String(s)
  };
};

let replace_un_op = (op: op_un, use_mode: option(mode)): op_un => {
  switch (op, use_mode) {
  | (op, None) => op
  | (Int(op) | Nat(op), Some(Int)) => Int(op)
  | (Int(op) | Nat(op), Some(Nat)) => Nat(op)
  | (Bool(op), _) => Bool(op)
  | (Meta(op), _) => Meta(op)
  };
};

let replace_bin_op = (op: op_bin, use_mode: option(mode)): op_bin => {
  switch (op, use_mode) {
  | (op, None) => op
  | (Int(op) | Nat(op), Some(Int)) => Int(op)
  | (Int(op) | Nat(op), Some(Nat)) => Nat(op)
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

let show_op_un_int: op_un_int => string =
  fun
  | Minus => "Integer Negation";

let show_unop: op_un => string =
  fun
  | Meta(op) => show_op_un_meta(op)
  | Bool(op) => show_op_un_bool(op)
  | Nat(op)
  | Int(op) => show_op_un_int(op);

let show_op_bin_bool: op_bin_bool => string =
  fun
  | And => "Boolean Conjunction"
  | Or => "Boolean Disjunction";

let show_op_bin_int: op_bin_int => string =
  fun
  | Plus => "Integer Addition"
  | Minus => "Integer Subtraction"
  | Times => "Integer Multiplication"
  | Power => "Integer Exponentiation"
  | Divide => "Integer Division"
  | LessThan => "Integer Less Than"
  | LessThanOrEqual => "Integer Less Than or Equal"
  | GreaterThan => "Integer Greater Than"
  | GreaterThanOrEqual => "Integer Greater Than or Equal"
  | Equals => "Integer Equality"
  | NotEquals => "Integer Inequality";

let show_op_bin_float: op_bin_float => string =
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
  | Int(op) => show_op_bin_int(op)
  | Nat(op) => show_op_bin_int(op)
  | Float(op) => show_op_bin_float(op)
  | Bool(op) => show_op_bin_bool(op)
  | String(op) => show_op_bin_string(op);

let bool_op_to_string = (op: op_bin_bool): string => {
  switch (op) {
  | And => "&&"
  | Or => "||"
  };
};

let int_op_to_string = (op: op_bin_int): string => {
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

let float_op_to_string = (op: op_bin_float): string => {
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
  | Int(op) => int_op_to_string(op)
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
  | Undefined;

let just = (f, x) => Either.L(f(x));

let semantics_of_un_op = (op: op_un): un_semantics =>
  switch (op) {
  | Int(Minus) => Defined(Int, Int, just(x => - x))
  | Nat(Minus) => Undefined
  | Bool(Not) => Defined(Bool, Bool, just(x => !x))
  | Meta(Unquote) => Undefined
  };

type bin_semantics =
  | Defined(
      Atom.kind('a),
      Atom.kind('b),
      Atom.kind('c),
      ('a, 'b) => Either.t('c, InvalidOperationError.t),
    )
    : bin_semantics
  | Undefined;

let just = (f, x, y) => Either.L(f(x, y));
let int_power = (x, y) =>
  if (y < 0) {
    Either.R(InvalidOperationError.NegativeExponent);
  } else {
    Either.L(IntUtil.ipow(x, y));
  };
let int_divide = (x, y) =>
  if (y === 0) {
    Either.R(InvalidOperationError.DivideByZero);
  } else {
    Either.L(x / y);
  };

let semantics_of_bin_op = (op: op_bin): bin_semantics =>
  switch (op) {
  | Int(Plus) => Defined(Int, Int, Int, just((+)))
  | Int(Minus) => Defined(Int, Int, Int, just((-)))
  | Int(Times) => Defined(Int, Int, Int, just(( * )))
  | Int(Power) => Defined(Int, Int, Int, int_power)
  | Int(Divide) => Defined(Int, Int, Int, int_divide)
  | Int(LessThan) => Defined(Int, Int, Bool, just((<)))
  | Int(LessThanOrEqual) => Defined(Int, Int, Bool, just((<=)))
  | Int(GreaterThan) => Defined(Int, Int, Bool, just((>)))
  | Int(GreaterThanOrEqual) => Defined(Int, Int, Bool, just((>=)))
  | Int(Equals) => Defined(Int, Int, Bool, just((==)))
  | Int(NotEquals) => Defined(Int, Int, Bool, just((!=)))

  | Nat(Plus) => Defined(Nat, Nat, Nat, just((+)))
  | Nat(Minus) => Undefined
  | Nat(Times) => Defined(Nat, Nat, Nat, just(( * )))
  | Nat(Power) => Defined(Nat, Nat, Nat, just(IntUtil.ipow))
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
         | Undefined => None
         | Defined(x, y, z, f) => Some((op_name(op), TwoFun(x, y, z, f)))
         }
       )
  );
};
