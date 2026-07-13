open Language;
open Haz3lcore;

let parens = value => "(" ++ value ++ ")";

let is_integer_float = value => value == Float.round(value);

let is_float_pi = value => abs_float(value -. Float.pi) < 0.000001;

let string_of_float_literal = value =>
  is_integer_float(value)
    ? string_of_int(int_of_float(value)) : string_of_float(value);

let op_to_algebrite =
  fun
  | Operators.Int(Operators.Plus)
  | Nat(Plus)
  | SInt(Plus)
  | Float(Plus) => Some("+")
  | Operators.Int(Operators.Minus)
  | SInt(Minus)
  | Float(Minus) => Some("-")
  | Operators.Int(Operators.Times)
  | Nat(Times)
  | SInt(Times)
  | Float(Times) => Some("*")
  | Operators.Int(Operators.Divide)
  | Nat(Divide)
  | SInt(Divide)
  | Float(Divide) => Some("/")
  | Operators.Int(Operators.Power)
  | Nat(Power)
  | SInt(Power)
  | Float(Power) => Some("^")
  | _ => None;

let function_name = exp => {
  let exp = exp |> DHExp.strip_ascriptions;
  switch (exp.term) {
  | Var(("sin" | "cos" | "tan") as name)
  | BuiltinFun(("sin" | "cos" | "tan") as name) => Some(name)
  | _ => None
  };
};

let rec serialize_for_algebrite = (exp: Exp.t): option(string) => {
  let exp = exp |> DHExp.strip_ascriptions;
  switch (exp.term) {
  | Parens(inner)
  | Asc(inner, _) => serialize_for_algebrite(inner)
  | Atom(Int(value))
  | Atom(Nat(value)) => Some(Bigint.to_string(value))
  | Atom(SInt(value)) => Some(string_of_int(value))
  | Atom(Float(value)) when is_float_pi(value) => Some("pi")
  | Atom(Float(value)) => Some(string_of_float_literal(value))
  | Var(name) => Some(name)
  | UnOp(
      Operators.Int(Operators.Minus) | SInt(Minus) | Float(Minus),
      inner,
    ) =>
    serialize_for_algebrite(inner)
    |> Option.map(value => "(-" ++ value ++ ")")
  | BinOp(op, left, right) =>
    switch (
      op_to_algebrite(op),
      serialize_for_algebrite(left),
      serialize_for_algebrite(right),
    ) {
    | (Some(op), Some(left), Some(right)) =>
      Some(parens(left ++ " " ++ op ++ " " ++ right))
    | _ => None
    }
  | Ap(Operators.Forward, fn, arg) =>
    switch (function_name(fn), serialize_for_algebrite(arg)) {
    | (Some(name), Some(arg)) => Some(name ++ parens(arg))
    | _ => None
    }
  | _ => None
  };
};

let is_nonnegative_integer_literal = exp => {
  let exp = exp |> DHExp.strip_ascriptions;
  switch (exp.term) {
  | Atom(Int(value))
  | Atom(Nat(value)) => Bigint.(>=)(value, Bigint.zero)
  | Atom(SInt(value)) => value >= 0
  | Atom(Float(value)) => value >= 0.0 && is_integer_float(value)
  | _ => false
  };
};

let rec is_polynomial = exp => {
  let exp = exp |> DHExp.strip_ascriptions;
  switch (exp.term) {
  | Parens(inner)
  | Asc(inner, _) => is_polynomial(inner)
  | Atom(Int(_))
  | Atom(Nat(_))
  | Atom(SInt(_))
  | Atom(Float(_))
  | Var(_) => true
  | UnOp(
      Operators.Int(Operators.Minus) | SInt(Minus) | Float(Minus),
      inner,
    ) =>
    is_polynomial(inner)
  | BinOp(op, base, exponent) when op_to_algebrite(op) == Some("^") =>
    is_polynomial(base) && is_nonnegative_integer_literal(exponent)
  | BinOp(op, left, right) =>
    List.mem(op_to_algebrite(op), [Some("+"), Some("-"), Some("*")])
    && is_polynomial(left)
    && is_polynomial(right)
  | _ => false
  };
};

let rec contains_variable = exp => {
  let exp = exp |> DHExp.strip_ascriptions;
  switch (exp.term) {
  | Var(_) => true
  | Parens(inner)
  | Asc(inner, _)
  | UnOp(_, inner) => contains_variable(inner)
  | BinOp(_, left, right) =>
    contains_variable(left) || contains_variable(right)
  | _ => false
  };
};

let nonnegative_integer_value = exp => {
  let exp = exp |> DHExp.strip_ascriptions;
  switch (exp.term) {
  | Atom(Int(value))
  | Atom(Nat(value)) =>
    Bigint.(>=)(value, Bigint.zero) ? Bigint.to_int(value) : None
  | Atom(SInt(value)) => value >= 0 ? Some(value) : None
  | _ => None
  };
};

let rec integer_polynomial_degree = exp => {
  let exp = exp |> DHExp.strip_ascriptions;
  switch (exp.term) {
  | Parens(inner)
  | Asc(inner, _) => integer_polynomial_degree(inner)
  | Atom(Int(_))
  | Atom(Nat(_))
  | Atom(SInt(_)) => Some(0)
  | Var(_) => Some(1)
  | UnOp(Operators.Int(Operators.Minus) | SInt(Minus), inner) =>
    integer_polynomial_degree(inner)
  | BinOp(op, base, exponent) when op_to_algebrite(op) == Some("^") =>
    switch (
      integer_polynomial_degree(base),
      nonnegative_integer_value(exponent),
    ) {
    | (Some(base_degree), Some(exponent)) => Some(base_degree * exponent)
    | _ => None
    }
  | BinOp(op, left, right) =>
    switch (
      op_to_algebrite(op),
      integer_polynomial_degree(left),
      integer_polynomial_degree(right),
    ) {
    | (Some("+" | "-"), Some(left_degree), Some(right_degree)) =>
      Some(max(left_degree, right_degree))
    | (Some("*"), Some(left_degree), Some(right_degree)) =>
      Some(left_degree + right_degree)
    | _ => None
    }
  | _ => None
  };
};

let rec variable_names = exp => {
  let exp = exp |> DHExp.strip_ascriptions;
  switch (exp.term) {
  | Var(name) => [name]
  | Parens(inner)
  | Asc(inner, _)
  | UnOp(_, inner) => variable_names(inner)
  | BinOp(_, left, right) => variable_names(left) @ variable_names(right)
  | _ => []
  };
};

let is_factor_candidate_shape = exp => {
  let exp = exp |> DHExp.strip_ascriptions;
  let is_expanded_sum =
    switch (exp.term) {
    | BinOp(op, _, _) =>
      List.mem(op_to_algebrite(op), [Some("+"), Some("-")])
    | _ => false
    };
  let is_supported_degree =
    switch (integer_polynomial_degree(exp)) {
    | Some(degree) => degree >= 1 && degree <= 2
    | None => false
    };
  let has_one_variable =
    variable_names(exp) |> List.sort_uniq(compare) |> List.length == 1;
  is_expanded_sum
  && has_one_variable
  && is_supported_degree
  && contains_variable(exp)
  && is_polynomial(exp);
};

let replace_all = (needle, replacement, input) =>
  Str.global_replace(Str.regexp_string(needle), replacement, input);

let hazel_syntax_of_algebrite = value =>
  value |> String.trim |> replace_all("^", "**");

let editor_of_hazel_text = (~settings as _, text) =>
  text
  |> Parser.to_zipper(~root=Exp)
  |> Option.map(zipper =>
       CodeEditable.Model.mk(Editor.Model.mk(zipper, ~root=Exp))
     );
