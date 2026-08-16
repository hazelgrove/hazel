open Language;

type op_kind =
  | Add
  | Sub
  | Mul
  | Div;

let strip = exp => {
  let rec loop = exp =>
    switch ((exp |> DHExp.strip_ascriptions).term) {
    | Parens(inner)
    | Asc(inner, _) => loop(inner)
    | _ => exp |> DHExp.strip_ascriptions
    };
  loop(exp);
};

let int_exp = value => Exp.fresh(Atom(Int(Bigint.of_int(value))));
let plus_exp = (left, right) =>
  Exp.fresh(BinOp(Operators.Int(Operators.Plus), left, right));
let minus_exp = (left, right) =>
  Exp.fresh(BinOp(Operators.Int(Operators.Minus), left, right));
let times_exp = (left, right) =>
  Exp.fresh(BinOp(Operators.Int(Operators.Times), left, right));
let divide_exp = (left, right) =>
  Exp.fresh(BinOp(Operators.Int(Operators.Divide), left, right));
let neg_exp = exp => Exp.fresh(UnOp(Operators.Int(Operators.Minus), exp));

type rational_constant = {
  numerator: int,
  denominator: int,
};

let rec rational_gcd = (left, right) => {
  let left = abs(left);
  let right = abs(right);
  right == 0 ? left : rational_gcd(right, left mod right);
};

let normalize_rational = (numerator, denominator): option(rational_constant) =>
  if (denominator == 0) {
    None;
  } else if (numerator == 0) {
    Some({
      numerator: 0,
      denominator: 1,
    });
  } else {
    let sign = denominator < 0 ? (-1) : 1;
    let divisor = rational_gcd(numerator, denominator);
    Some({
      numerator: numerator / divisor * sign,
      denominator: abs(denominator) / divisor,
    });
  };

let int_constant = exp => {
  let exp = strip(exp);
  switch (exp.term) {
  | Atom(Int(value))
  | Atom(Nat(value)) => Bigint.to_int(value)
  | Atom(SInt(value)) => Some(value)
  | Atom(Real(Real.Rational({numerator, denominator, _})))
      when Bigint.equal(denominator, Bigint.one) =>
    Bigint.to_int(numerator)
  | Atom(Float(value)) when value == Float.round(value) =>
    Some(int_of_float(value))
  | _ => None
  };
};

let op_matches = (kind, op) =>
  switch (kind, op) {
  | (
      Add,
      Operators.Int(Operators.Plus) | Nat(Plus) | SInt(Plus) | Float(Plus) |
      Real(Plus),
    ) =>
    true
  | (
      Sub,
      Operators.Int(Operators.Minus) | SInt(Minus) | Float(Minus) |
      Real(Minus),
    ) =>
    true
  | (
      Mul,
      Operators.Int(Operators.Times) | Nat(Times) | SInt(Times) |
      Float(Times) |
      Real(Times),
    ) =>
    true
  | (
      Div,
      Operators.Int(Operators.Divide) | Nat(Divide) | SInt(Divide) |
      Float(Divide) |
      Real(Divide),
    ) =>
    true
  | _ => false
  };

let is_power_op = op =>
  switch (op) {
  | Operators.Int(Operators.Power)
  | Nat(Power)
  | SInt(Power)
  | Float(Power)
  | Real(Power) => true
  | _ => false
  };

let add_rational = (left, right) =>
  normalize_rational(
    left.numerator * right.denominator + right.numerator * left.denominator,
    left.denominator * right.denominator,
  );

let subtract_rational = (left, right) =>
  normalize_rational(
    left.numerator * right.denominator - right.numerator * left.denominator,
    left.denominator * right.denominator,
  );

let multiply_rational = (left, right) =>
  normalize_rational(
    left.numerator * right.numerator,
    left.denominator * right.denominator,
  );

let divide_rational = (left, right) =>
  right.numerator == 0
    ? None
    : normalize_rational(
        left.numerator * right.denominator,
        left.denominator * right.numerator,
      );

let rec power_rational = (base, exponent) =>
  exponent == 0
    ? Some({
        numerator: 1,
        denominator: 1,
      })
    : (
      switch (power_rational(base, exponent - 1)) {
      | Some(acc) => multiply_rational(base, acc)
      | None => None
      }
    );

/* A rational literal is an integer or a displayed quotient of integers. It is
   intentionally narrower than an arbitrary constant expression so one
   visible step cannot collapse, for example, `2 * (1 + 2)` all the way to 6. */
let rec rational_literal_of_exp = exp => {
  let exp = strip(exp);
  switch (int_constant(exp), exp.term) {
  | (Some(value), _) => normalize_rational(value, 1)
  | (
      None,
      UnOp(
        Operators.Int(Operators.Minus) | Nat(Minus) | SInt(Minus) |
        Float(Minus) |
        Real(Minus),
        inner,
      ),
    ) =>
    rational_literal_of_exp(inner)
    |> Option.bind(_, value =>
         normalize_rational(- value.numerator, value.denominator)
       )
  | (None, BinOp(op, left, right)) when op_matches(Div, op) =>
    switch (int_constant(left), int_constant(right)) {
    | (Some(left), Some(right)) => normalize_rational(left, right)
    | _ => None
    }
  | _ => None
  };
};

let exp_of_rational_constant = (~real_mode, value) =>
  if (real_mode) {
    Exp.fresh(
      Atom(
        Real(
          Real.normalize(
            Bigint.of_int(value.numerator),
            Bigint.of_int(value.denominator),
            None,
          ),
        ),
      ),
    );
  } else {
    let numerator = int_exp(value.numerator);
    value.denominator == 1
      ? numerator : divide_exp(numerator, int_exp(value.denominator));
  };

/* Exact, deliberately bounded evaluation for the existing `Evaluate
   constants` capability. This covers classroom-sized rational operations;
   it is not a separate shortcut for any particular worked example. */
let fold_rational_constant = exp => {
  let exp = strip(exp);
  let real_mode =
    switch (exp.term) {
    | Atom(Real(_))
    | BinOp(Real(_), _, _)
    | UnOp(Real(_), _) => true
    | _ => false
    };
  let folded =
    switch (exp.term) {
    | BinOp(op, left, right) when op_matches(Add, op) =>
      switch (rational_literal_of_exp(left), rational_literal_of_exp(right)) {
      | (Some(left), Some(right)) => add_rational(left, right)
      | _ => None
      }
    | BinOp(op, left, right) when op_matches(Sub, op) =>
      switch (rational_literal_of_exp(left), rational_literal_of_exp(right)) {
      | (Some(left), Some(right)) => subtract_rational(left, right)
      | _ => None
      }
    | BinOp(op, left, right) when op_matches(Mul, op) =>
      switch (rational_literal_of_exp(left), rational_literal_of_exp(right)) {
      | (Some(left), Some(right)) => multiply_rational(left, right)
      | _ => None
      }
    | BinOp(op, left, right) when op_matches(Div, op) =>
      switch (rational_literal_of_exp(left), rational_literal_of_exp(right)) {
      | (Some(left), Some(right)) => divide_rational(left, right)
      | _ => None
      }
    | BinOp(op, base, exponent) when is_power_op(op) =>
      switch (rational_literal_of_exp(base), int_constant(exponent)) {
      | (Some(base), Some(exponent)) when exponent >= 0 && exponent <= 8 =>
        power_rational(base, exponent)
      | _ => None
      }
    | _ => None
    };
  folded |> Option.map(exp_of_rational_constant(~real_mode));
};

let rec flatten_mul = exp => {
  let exp = strip(exp);
  switch (exp.term) {
  | BinOp(op, left, right) when op_matches(Mul, op) =>
    flatten_mul(left) @ flatten_mul(right)
  | _ => [exp]
  };
};

let rebuild_positive_product = (coefficient, factors) =>
  switch (coefficient, factors) {
  | (0, _) => int_exp(0)
  | (1, []) => int_exp(1)
  | (1, [factor]) => factor
  | (1, [first, second, ...rest]) =>
    List.fold_left(
      (acc, factor) => times_exp(acc, factor),
      times_exp(first, second),
      rest,
    )
  | (_, []) => int_exp(coefficient)
  | (_, factors) =>
    List.fold_left(
      (acc, factor) => times_exp(acc, factor),
      int_exp(coefficient),
      factors,
    )
  };

let rebuild_product = (coefficient, factors) =>
  coefficient < 0
    ? neg_exp(rebuild_positive_product(- coefficient, factors))
    : rebuild_positive_product(coefficient, factors);

let gcd = rational_gcd;

let negative_operand = exp =>
  switch (strip(exp).term) {
  | UnOp(
      Operators.Int(Operators.Minus) | Nat(Minus) | SInt(Minus) |
      Float(Minus),
      inner,
    ) =>
    Some(inner)
  | _ => None
  };

let product_parts = factors => {
  let rec collect = (coefficient, symbolic_factors, remaining) =>
    switch (remaining) {
    | [] => (coefficient, List.rev(symbolic_factors))
    | [factor, ...rest] =>
      switch (int_constant(factor), negative_operand(factor)) {
      | (Some(value), _) =>
        collect(coefficient * value, symbolic_factors, rest)
      | (None, Some(inner)) =>
        collect(- coefficient, symbolic_factors, flatten_mul(inner) @ rest)
      | (None, None) =>
        collect(coefficient, [factor, ...symbolic_factors], rest)
      }
    };
  collect(1, [], factors);
};

let simplify_scalar_quotient = (numerator, denominator) =>
  switch (int_constant(denominator)) {
  | Some(denominator) when denominator != 0 =>
    let (coefficient, factors) = numerator |> flatten_mul |> product_parts;
    let sign = denominator < 0 ? (-1) : 1;
    let denominator = abs(denominator);
    let divisor = gcd(coefficient, denominator);
    let coefficient = coefficient / divisor * sign;
    let denominator = denominator / divisor;
    let numerator = rebuild_product(coefficient, factors);
    denominator == 1
      ? numerator : divide_exp(numerator, int_exp(denominator));
  | Some(_)
  | None => divide_exp(numerator, denominator)
  };

let rec simplify_product = exp => {
  let factors = flatten_mul(exp) |> List.map(simplify_scalar_products);
  let (coefficient, symbolic_factors) = product_parts(factors);
  rebuild_product(coefficient, symbolic_factors);
}

and simplify_scalar_products = exp => {
  let exp = strip(exp);
  switch (exp.term) {
  | BinOp(op, _, _) when op_matches(Mul, op) => simplify_product(exp)
  | BinOp(op, left, right) when op_matches(Div, op) =>
    simplify_scalar_quotient(
      simplify_scalar_products(left),
      simplify_scalar_products(right),
    )
  | BinOp(op, left, right) when op_matches(Sub, op) =>
    let left = simplify_scalar_products(left);
    let right = simplify_scalar_products(right);
    switch (negative_operand(right)) {
    | Some(positive) =>
      int_constant(left) == Some(0) ? positive : plus_exp(left, positive)
    | None => minus_exp(left, right)
    };
  | BinOp(op, left, right) when op_matches(Add, op) =>
    let left = simplify_scalar_products(left);
    let right = simplify_scalar_products(right);
    switch (negative_operand(right)) {
    | Some(positive) => minus_exp(left, positive)
    | None => plus_exp(left, right)
    };
  | BinOp(op, left, right) =>
    Exp.fresh(
      BinOp(
        op,
        simplify_scalar_products(left),
        simplify_scalar_products(right),
      ),
    )
  | UnOp(
      (
        Operators.Int(Operators.Minus) | Nat(Minus) | SInt(Minus) |
        Float(Minus)
      ) as op,
      inner,
    ) =>
    let inner = simplify_scalar_products(inner);
    switch (negative_operand(inner)) {
    | Some(positive) => positive
    | None => Exp.fresh(UnOp(op, inner))
    };
  | UnOp(op, inner) => Exp.fresh(UnOp(op, simplify_scalar_products(inner)))
  | Ap(dir, fn, arg) =>
    Exp.fresh(
      Ap(dir, simplify_scalar_products(fn), simplify_scalar_products(arg)),
    )
  | Parens(inner) => Exp.fresh(Parens(simplify_scalar_products(inner)))
  | Asc(inner, typ) => Exp.fresh(Asc(simplify_scalar_products(inner), typ))
  | Projector(label, inner) =>
    Exp.fresh(Projector(label, simplify_scalar_products(inner)))
  | _ => exp
  };
};
