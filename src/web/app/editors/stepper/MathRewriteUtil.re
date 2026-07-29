open Language;
open Util;

let dedup = values =>
  values
  |> List.fold_left(
       (acc, value) => List.mem(value, acc) ? acc : [value, ...acc],
       [],
     )
  |> List.rev;

let rec strip_math_wrappers = (exp: Exp.t) =>
  switch (exp.term) {
  | Parens(exp)
  | Asc(exp, _) => strip_math_wrappers(exp)
  | _ => exp
  };

let exp_same = (left, right) =>
  Exp.fast_equal(strip_math_wrappers(left), strip_math_wrappers(right));

let int_exp = value => Exp.fresh(Atom(Int(value)));

let plus_exp = (left, right) =>
  Exp.fresh(BinOp(Operators.Int(Operators.Plus), left, right));

let times_exp = (left, right) =>
  Exp.fresh(BinOp(Operators.Int(Operators.Times), left, right));

let negate_exp = exp =>
  Exp.fresh(UnOp(Operators.Int(Operators.Minus), exp));

let is_plus_op =
  fun
  | Operators.Int(Operators.Plus)
  | Nat(Plus)
  | SInt(Plus)
  | Float(Plus) => true
  | _ => false;

let is_minus_op =
  fun
  | Operators.Int(Operators.Minus)
  | SInt(Minus)
  | Float(Minus) => true
  | _ => false;

let is_times_op =
  fun
  | Operators.Int(Operators.Times)
  | Nat(Times)
  | SInt(Times)
  | Float(Times) => true
  | _ => false;

let is_divide_op =
  fun
  | Operators.Int(Operators.Divide)
  | Nat(Divide)
  | SInt(Divide)
  | Float(Divide) => true
  | _ => false;

let plus_exp_with_op = (op, left, right) =>
  Exp.fresh(BinOp(op, left, right));

let times_exp_with_op = (op, left, right) =>
  Exp.fresh(BinOp(op, left, right));

let int_constant = exp => {
  let exp = exp |> DHExp.strip_ascriptions |> strip_math_wrappers;
  switch (exp.term) {
  | Atom(Int(value))
  | Atom(Nat(value)) => Some(value)
  | Atom(SInt(value)) => Some(Bigint.of_int(value))
  | _ => None
  };
};

let factors_of_product = exp => {
  let exp = strip_math_wrappers(exp);
  switch (exp.term) {
  | BinOp(times_op, left, right) when is_times_op(times_op) =>
    Some((times_op, strip_math_wrappers(left), strip_math_wrappers(right)))
  | _ => None
  };
};

let exp_compare = (left, right) =>
  String.compare(Exp.show(left), Exp.show(right));

let normalized_sum = (plus_op, left, right) => {
  let (left, right) =
    exp_compare(left, right) <= 0 ? (left, right) : (right, left);
  plus_exp_with_op(plus_op, left, right);
};

let distributed_additive_exp = (add_op, left, right) =>
  switch (add_op) {
  | Operators.Int(Operators.Plus)
  | Nat(Plus)
  | SInt(Plus) => normalized_sum(add_op, left, right)
  | Operators.Int(Operators.Minus)
  | SInt(Minus) => plus_exp_with_op(add_op, left, right)
  | _ => normalized_sum(add_op, left, right)
  };

let distribute_div_over_add_candidates = (exp: Exp.t): list(Exp.t) => {
  let exp = strip_math_wrappers(exp);
  switch (exp.term) {
  | BinOp(divide_op, numerator, denominator) when is_divide_op(divide_op) =>
    switch (strip_math_wrappers(numerator).term) {
    | BinOp(add_op, left, right)
        when is_plus_op(add_op) || is_minus_op(add_op) =>
      let left_quotient = Exp.fresh(BinOp(divide_op, left, denominator));
      let right_quotient = Exp.fresh(BinOp(divide_op, right, denominator));
      [distributed_additive_exp(add_op, left_quotient, right_quotient)];
    | _ => []
    }
  | _ => []
  };
};
