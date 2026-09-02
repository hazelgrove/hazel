open Language;

type normalization = {
  exp: Exp.t,
  steps: list(TrigRewrite.rewrite),
  complete: bool,
};

let strip = TrigRewrite.strip;
let exp_same = TrigRewrite.exp_same;

let int_exp = value => Exp.fresh(Atom(Int(Bigint.of_int(value))));
let var_exp = name => Exp.fresh(Var(name));
let app_exp = (name, arg) =>
  Exp.fresh(Ap(Operators.Forward, var_exp(name), arg));
let trig_app_exp = (~real_math, name, arg) =>
  app_exp(
    TrigRewrite.function_name_with_style(
      real_math ? TrigRewrite.RealMath : TrigRewrite.IntegerMath,
      name,
    ),
    arg,
  );
let tuple_exp = entries => Exp.fresh(Tuple(entries));
let plus_exp = (left, right) =>
  Exp.fresh(BinOp(Operators.Int(Operators.Plus), left, right));
let minus_exp = (left, right) =>
  Exp.fresh(BinOp(Operators.Int(Operators.Minus), left, right));
let times_exp = (left, right) =>
  Exp.fresh(BinOp(Operators.Int(Operators.Times), left, right));
let divide_exp = (left, right) =>
  Exp.fresh(BinOp(Operators.Int(Operators.Divide), left, right));
let power_exp = (left, right) =>
  Exp.fresh(BinOp(Operators.Int(Operators.Power), left, right));
let neg_exp = exp => Exp.fresh(UnOp(Operators.Int(Operators.Minus), exp));
let neg_exp_for_operator = (op, exp) =>
  Exp.fresh(
    UnOp(
      switch (op) {
      | Operators.Float(Operators.Minus) => Operators.Float(Operators.Minus)
      | SInt(Minus) => SInt(Minus)
      | _ => Operators.Int(Operators.Minus)
      },
      exp,
    ),
  );
let diff_exp = (expression, variable) =>
  DerivativeOperator.expression(~body=expression, ~variable);
let legacy_diff_exp = (expression, variable) =>
  app_exp(DerivativeOperator.legacy_name, tuple_exp([expression, variable]));
let function_diff_exp = expression =>
  app_exp(DerivativeOperator.legacy_name, expression);

let function_name = exp => {
  let exp = strip(exp);
  switch (exp.term) {
  | Var(name)
  | BuiltinFun(name) =>
    switch (name) {
    | "sin_real" => Some("sin")
    | "cos_real" => Some("cos")
    | "tan_real" => Some("tan")
    | name => Some(name)
    }
  | _ => None
  };
};

let diff_parts = DerivativeOperator.expression_parts;

let function_diff_argument = DerivativeOperator.function_argument;

let derivative_builder = source =>
  Option.is_some(DerivativeOperator.expression_parts(~legacy=false, source))
  || Option.is_some(
       DerivativeOperator.function_argument(~legacy=false, source),
     )
    ? diff_exp : legacy_diff_exp;

let variable_name = exp =>
  switch (strip(exp).term) {
  | Var(name) => Some(name)
  | _ => None
  };

let rec function_parameter_name = (pattern: Pat.t) =>
  switch (pattern.term) {
  | Tuple([parameter]) => function_parameter_name(parameter)
  | Ap({term: Var(_), _}, parameter) => function_parameter_name(parameter)
  | _ => Pat.get_var(pattern)
  };

let is_operator = (expected, actual) =>
  switch (expected, actual) {
  | (
      Operators.Plus,
      Operators.Int(Plus) | Nat(Plus) | SInt(Plus) | Real(Plus) |
      Float(Plus),
    )
  | (
      Operators.Minus,
      Operators.Int(Minus) | SInt(Minus) | Real(Minus) | Float(Minus),
    )
  | (
      Operators.Times,
      Operators.Int(Times) | Nat(Times) | SInt(Times) | Real(Times) |
      Float(Times),
    )
  | (
      Operators.Divide,
      Operators.Int(Divide) | Nat(Divide) | SInt(Divide) | Real(Divide) |
      Float(Divide),
    )
  | (
      Operators.Power,
      Operators.Int(Power) | Nat(Power) | SInt(Power) | Real(Power) |
      Float(Power),
    ) =>
    true
  | _ => false
  };

let rec depends_on = (variable, exp) => {
  let exp = strip(exp);
  switch (exp.term) {
  | Var(name) => name == variable
  | Atom(_)
  | BuiltinFun(_)
  | EmptyHole
  | MultiHole(_)
  | Invalid(_)
  | Undefined => false
  | BinOp(_, left, right)
  | Ap(_, left, right) =>
    depends_on(variable, left) || depends_on(variable, right)
  | UnOp(_, inner)
  | Parens(inner)
  | Asc(inner, _)
  | Projector(_, inner) => depends_on(variable, inner)
  | Tuple(entries)
  | ListLit(entries) => List.exists(depends_on(variable), entries)
  | Fun(pat, body, _, _) =>
    switch (function_parameter_name(pat)) {
    | Some(bound) when bound == variable => false
    | _ => depends_on(variable, body)
    }
  | _ => true
  };
};

let integer_constant = exp => {
  let exp = strip(exp);
  switch (exp.term) {
  | Atom(Int(value))
  | Atom(Nat(value)) => Bigint.to_int(value)
  | Atom(SInt(value)) => Some(value)
  | Atom(Float(value)) when value == Float.round(value) =>
    Some(int_of_float(value))
  | Atom(Real(Real.Rational({numerator, denominator, _})))
      when Bigint.equal(denominator, Bigint.one) =>
    Bigint.to_int(numerator)
  | _ => None
  };
};

let rewrite = (~rule_id, ~label, ~before_exp, ~after_exp) =>
  TrigRewrite.{
    rule_id,
    label,
    before_exp,
    after_exp,
  };

let enabled = (~rule_enabled, rule_id) => rule_enabled(rule_id);

let rec distribute_diff_over_operator =
        (~operator, ~combine, ~make_diff, expression, variable) => {
  let expression = strip(expression);
  switch (expression.term) {
  | BinOp(op, left, right) when is_operator(operator, op) =>
    combine(
      distribute_diff_over_operator(
        ~operator,
        ~combine,
        ~make_diff,
        left,
        variable,
      ),
      distribute_diff_over_operator(
        ~operator,
        ~combine,
        ~make_diff,
        right,
        variable,
      ),
    )
  | _ => make_diff(expression, variable)
  };
};

let applicable_at_root_with_context =
    (~rule_enabled, ~float_context, ~real_context, exp) => {
  let before_exp = strip(exp);
  let make_diff = derivative_builder(before_exp);
  let make = (rule_id, label, after_exp) =>
    enabled(~rule_enabled, rule_id)
      ? [rewrite(~rule_id, ~label, ~before_exp, ~after_exp)] : [];
  switch (function_diff_argument(before_exp)) {
  | Some(function_exp) =>
    switch (strip(function_exp).term) {
    | Fun(pat, body, return_typ, name) =>
      switch (function_parameter_name(pat)) {
      | Some(parameter) =>
        make(
          "calc.diff_function_value",
          "differentiate function",
          Exp.fresh(
            Fun(
              pat,
              make_diff(strip(body), var_exp(parameter)),
              return_typ,
              name,
            ),
          ),
        )
      | None => []
      }
    | _ => []
    }
  | None =>
    switch (diff_parts(before_exp)) {
    | None => []
    | Some((expression, variable)) =>
      switch (variable_name(variable)) {
      | None => []
      | Some(variable_name) =>
        let expression = strip(expression);
        let float_math =
          float_context || TrigRewrite.uses_float_math(expression);
        let real_math =
          !float_math
          && (real_context || TrigRewrite.uses_real_math(expression));
        let literal = value =>
          float_math
            ? TrigRewrite.float_exp(float_of_int(value))
            : real_math
                ? Exp.fresh(
                    Atom(Real(Real.of_bigint(Bigint.of_int(value)))),
                  )
                : int_exp(value);
        let numeric_bin_op = (int_op, real_op, float_op) =>
          float_math ? float_op : real_math ? real_op : int_op;
        let numeric_un_op =
            (
              int_op: Operators.op_un,
              real_op: Operators.op_un,
              float_op: Operators.op_un,
            )
            : Operators.op_un =>
          float_math ? float_op : real_math ? real_op : int_op;
        let plus = (left, right) =>
          Exp.fresh(
            BinOp(
              numeric_bin_op(
                Operators.Int(Operators.Plus),
                Operators.Real(Operators.Plus),
                Operators.Float(Operators.Plus),
              ),
              left,
              right,
            ),
          );
        let minus = (left, right) =>
          Exp.fresh(
            BinOp(
              numeric_bin_op(
                Operators.Int(Operators.Minus),
                Operators.Real(Operators.Minus),
                Operators.Float(Operators.Minus),
              ),
              left,
              right,
            ),
          );
        let times = (left, right) =>
          Exp.fresh(
            BinOp(
              numeric_bin_op(
                Operators.Int(Operators.Times),
                Operators.Real(Operators.Times),
                Operators.Float(Operators.Times),
              ),
              left,
              right,
            ),
          );
        let divide = (left, right) =>
          Exp.fresh(
            BinOp(
              numeric_bin_op(
                Operators.Int(Operators.Divide),
                Operators.Real(Operators.Divide),
                Operators.Float(Operators.Divide),
              ),
              left,
              right,
            ),
          );
        let power = (left, right) =>
          Exp.fresh(
            BinOp(
              numeric_bin_op(
                Operators.Int(Operators.Power),
                Operators.Real(Operators.Power),
                Operators.Float(Operators.Power),
              ),
              left,
              right,
            ),
          );
        let negate = inner =>
          Exp.fresh(
            UnOp(
              numeric_un_op(
                Operators.Int(Operators.Minus),
                Operators.Real(Operators.Minus),
                Operators.Float(Operators.Minus),
              ),
              inner,
            ),
          );
        switch (expression.term) {
        | Fun(pat, body, _, _) =>
          switch (function_parameter_name(pat)) {
          | Some(bound_name) when bound_name == variable_name =>
            make(
              "calc.diff_function",
              "differentiate function body",
              make_diff(strip(body), variable),
            )
          | _ => []
          }
        | Var(name) when name == variable_name =>
          make("calc.diff_variable", "derivative of a variable", literal(1))
        | _ when !depends_on(variable_name, expression) =>
          make("calc.diff_constant", "derivative of a constant", literal(0))
        | Ap(Operators.Forward, fn, inner)
            when function_name(fn) == Some("sin") =>
          make(
            "calc.diff_chain_sin",
            "sine chain rule",
            times(
              trig_app_exp(~real_math, "cos", inner),
              make_diff(inner, variable),
            ),
          )
        | Ap(Operators.Forward, fn, inner)
            when function_name(fn) == Some("cos") =>
          make(
            "calc.diff_chain_cos",
            "cosine chain rule",
            times(
              negate(trig_app_exp(~real_math, "sin", inner)),
              make_diff(inner, variable),
            ),
          )
        | BinOp(op, _, _) when is_operator(Operators.Plus, op) =>
          make(
            "calc.diff_sum",
            "linearity (sum rule)",
            distribute_diff_over_operator(
              ~operator=Operators.Plus,
              ~combine=plus,
              ~make_diff,
              expression,
              variable,
            ),
          )
        | BinOp(op, _, _) when is_operator(Operators.Minus, op) =>
          make(
            "calc.diff_difference",
            "linearity (difference rule)",
            distribute_diff_over_operator(
              ~operator=Operators.Minus,
              ~combine=minus,
              ~make_diff,
              expression,
              variable,
            ),
          )
        | BinOp(op, left, right) when is_operator(Operators.Times, op) =>
          make(
            "calc.diff_product",
            "product rule",
            plus(
              times(make_diff(left, variable), right),
              times(left, make_diff(right, variable)),
            ),
          )
        | BinOp(op, numerator, denominator)
            when is_operator(Operators.Divide, op) =>
          make(
            "calc.diff_quotient",
            "quotient rule (denominator nonzero)",
            divide(
              minus(
                times(make_diff(numerator, variable), denominator),
                times(numerator, make_diff(denominator, variable)),
              ),
              power(denominator, literal(2)),
            ),
          )
        | BinOp(op, base, exponent) when is_operator(Operators.Power, op) =>
          switch (integer_constant(exponent)) {
          | Some(exponent_value) when exponent_value > 0 =>
            make(
              "calc.diff_power",
              "power rule",
              times(
                times(
                  literal(exponent_value),
                  power(base, literal(exponent_value - 1)),
                ),
                make_diff(base, variable),
              ),
            )
          | _ => []
          }
        | UnOp(
            Operators.Int(Operators.Minus) | SInt(Minus) | Real(Minus) |
            Float(Minus),
            inner,
          ) =>
          make(
            "calc.diff_negation",
            "derivative negation rule",
            negate(make_diff(inner, variable)),
          )
        | _ => []
        };
      }
    }
  };
};

let applicable_at_root = (~rule_enabled, exp) =>
  applicable_at_root_with_context(
    ~rule_enabled,
    ~float_context=TrigRewrite.uses_float_math(exp),
    ~real_context=TrigRewrite.uses_real_math(exp),
    exp,
  );

let rebuild_unary = (op, inner) => Exp.fresh(UnOp(op, inner));
let rebuild_binary = (op, left, right) => Exp.fresh(BinOp(op, left, right));

let rec rewrite_first_with_context =
        (~rule_enabled, ~float_context, ~real_context, exp) => {
  let float_context = float_context || TrigRewrite.uses_float_math(exp);
  let real_context =
    real_context || !float_context && TrigRewrite.uses_real_math(exp);
  switch (
    applicable_at_root_with_context(
      ~rule_enabled,
      ~float_context,
      ~real_context,
      exp,
    )
  ) {
  | [root, ..._] => Some((root.after_exp, root))
  | [] =>
    let exp = strip(exp);
    switch (exp.term) {
    | BinOp(op, left, right) =>
      switch (
        rewrite_first_with_context(
          ~rule_enabled,
          ~float_context,
          ~real_context,
          left,
        )
      ) {
      | Some((left, step)) => Some((rebuild_binary(op, left, right), step))
      | None =>
        rewrite_first_with_context(
          ~rule_enabled,
          ~float_context,
          ~real_context,
          right,
        )
        |> Option.map(((right, step)) =>
             (rebuild_binary(op, left, right), step)
           )
      }
    | UnOp(op, inner) =>
      rewrite_first_with_context(
        ~rule_enabled,
        ~float_context,
        ~real_context,
        inner,
      )
      |> Option.map(((inner, step)) => (rebuild_unary(op, inner), step))
    | Ap(direction, fn, arg) =>
      switch (
        rewrite_first_with_context(
          ~rule_enabled,
          ~float_context,
          ~real_context,
          fn,
        )
      ) {
      | Some((fn, step)) => Some((Exp.fresh(Ap(direction, fn, arg)), step))
      | None =>
        rewrite_first_with_context(
          ~rule_enabled,
          ~float_context,
          ~real_context,
          arg,
        )
        |> Option.map(((arg, step)) =>
             (Exp.fresh(Ap(direction, fn, arg)), step)
           )
      }
    | Tuple(entries) =>
      let rec rewrite_entry = (before, remaining) =>
        switch (remaining) {
        | [] => None
        | [entry, ...rest] =>
          switch (
            rewrite_first_with_context(
              ~rule_enabled,
              ~float_context,
              ~real_context,
              entry,
            )
          ) {
          | Some((entry, step)) =>
            Some((tuple_exp(List.rev(before) @ [entry, ...rest]), step))
          | None => rewrite_entry([entry, ...before], rest)
          }
        };
      rewrite_entry([], entries);
    | Parens(inner) =>
      rewrite_first_with_context(
        ~rule_enabled,
        ~float_context,
        ~real_context,
        inner,
      )
      |> Option.map(((inner, step)) => (Exp.fresh(Parens(inner)), step))
    | Asc(inner, typ) =>
      rewrite_first_with_context(
        ~rule_enabled,
        ~float_context,
        ~real_context,
        inner,
      )
      |> Option.map(((inner, step)) => (Exp.fresh(Asc(inner, typ)), step))
    | Projector(label, inner) =>
      rewrite_first_with_context(
        ~rule_enabled,
        ~float_context,
        ~real_context,
        inner,
      )
      |> Option.map(((inner, step)) =>
           (Exp.fresh(Projector(label, inner)), step)
         )
    | Fun(pat, body, return_typ, name) =>
      rewrite_first_with_context(
        ~rule_enabled,
        ~float_context,
        ~real_context,
        body,
      )
      |> Option.map(((body, step)) =>
           (Exp.fresh(Fun(pat, body, return_typ, name)), step)
         )
    | _ => None
    };
  };
};

let rewrite_first = (~rule_enabled, exp) =>
  rewrite_first_with_context(
    ~rule_enabled,
    ~float_context=false,
    ~real_context=false,
    exp,
  );

let normalize = (~rule_enabled, ~fuel=128, exp) => {
  let rec loop = (fuel, exp, steps) =>
    if (fuel <= 0) {
      {
        exp,
        steps: List.rev(steps),
        complete: false,
      };
    } else {
      switch (rewrite_first(~rule_enabled, exp)) {
      | Some((next, step)) => loop(fuel - 1, next, [step, ...steps])
      | None => {
          exp,
          steps: List.rev(steps),
          complete: true,
        }
      };
    };
  loop(fuel, strip(exp), []);
};

let rec contains_diff = exp =>
  if (Option.is_some(diff_parts(exp))
      || Option.is_some(function_diff_argument(exp))) {
    true;
  } else {
    switch (strip(exp).term) {
    | BinOp(_, left, right)
    | Ap(_, left, right) => contains_diff(left) || contains_diff(right)
    | UnOp(_, inner)
    | Parens(inner)
    | Asc(inner, _)
    | Projector(_, inner) => contains_diff(inner)
    | Tuple(entries)
    | ListLit(entries) => List.exists(contains_diff, entries)
    | Fun(_, body, _, _) => contains_diff(body)
    | _ => false
    };
  };

let is_zero = exp => integer_constant(exp) == Some(0);
let is_one = exp => integer_constant(exp) == Some(1);

let square_power = (times_op, base) => {
  let (power_op, exponent) =
    switch (times_op) {
    | Operators.Int(Operators.Times) => (
        Operators.Int(Operators.Power),
        int_exp(2),
      )
    | Nat(Times) => (Nat(Power), Exp.fresh(Atom(Nat(Bigint.of_int(2)))))
    | SInt(Times) => (SInt(Power), Exp.fresh(Atom(SInt(2))))
    | Float(Times) => (Float(Power), Exp.fresh(Atom(Float(2.0))))
    | Real(Times) => (
        Real(Power),
        Exp.fresh(Atom(Real(Real.of_bigint(Bigint.of_int(2))))),
      )
    | _ => (Operators.Int(Operators.Power), int_exp(2))
    };
  Exp.fresh(BinOp(power_op, base, exponent));
};

let negative_operand = exp =>
  switch (strip(exp).term) {
  | UnOp(
      Operators.Int(Operators.Minus) | SInt(Minus) | Real(Minus) |
      Float(Minus),
      inner,
    ) =>
    Some(inner)
  | _ => None
  };

let is_basic_cleanup_rule_id = rule_id =>
  rule_id == "calc.diff_constant" || rule_id == "calc.diff_variable";

let rec cleanup = (~cleanup_enabled, exp) => {
  let exp = strip(exp);
  let make_diff = derivative_builder(exp);
  let recurse = cleanup(~cleanup_enabled);
  switch (diff_parts(exp)) {
  | Some((expression, variable))
      when cleanup_enabled(Axioms.DerivativeBasics) =>
    let expression = recurse(expression);
    let variable = recurse(variable);
    switch (variable_name(variable)) {
    | Some(variable_name) =>
      switch (strip(expression).term) {
      | Var(name) when name == variable_name => int_exp(1)
      | Fun(_, _, _, _) => make_diff(expression, variable)
      | _ when !depends_on(variable_name, expression) => int_exp(0)
      | _ => make_diff(expression, variable)
      }
    | None => make_diff(expression, variable)
    };
  | _ =>
    switch (exp.term) {
    | BinOp(op, left, right) =>
      let left = recurse(left);
      let right = recurse(right);
      if (is_operator(Operators.Plus, op)) {
        if (cleanup_enabled(Axioms.AddIdentity) && is_zero(left)) {
          right;
        } else if (cleanup_enabled(Axioms.AddIdentity) && is_zero(right)) {
          left;
        } else if (cleanup_enabled(Axioms.CollectLikeTerms)
                   && exp_same(left, right)) {
          times_exp(int_exp(2), left);
        } else if (cleanup_enabled(Axioms.CollectLikeTerms)) {
          switch (negative_operand(right)) {
          | Some(right) => minus_exp(left, right)
          | None => rebuild_binary(op, left, right)
          };
        } else {
          rebuild_binary(op, left, right);
        };
      } else if (is_operator(Operators.Minus, op)) {
        if (cleanup_enabled(Axioms.AddIdentity) && is_zero(right)) {
          left;
        } else if (cleanup_enabled(Axioms.AddIdentity) && is_zero(left)) {
          neg_exp_for_operator(op, right);
        } else {
          rebuild_binary(op, left, right);
        };
      } else if (is_operator(Operators.Times, op)) {
        if (cleanup_enabled(Axioms.MulIdentity)
            && (is_zero(left) || is_zero(right))) {
          int_exp(0);
        } else if (cleanup_enabled(Axioms.MulIdentity) && is_one(left)) {
          right;
        } else if (cleanup_enabled(Axioms.MulIdentity) && is_one(right)) {
          left;
        } else if (cleanup_enabled(Axioms.PowerNotation)
                   && exp_same(left, right)) {
          square_power(op, left);
        } else {
          rebuild_binary(op, left, right);
        };
      } else if (is_operator(Operators.Power, op)) {
        if (cleanup_enabled(Axioms.PowerIdentity) && is_zero(right)) {
          int_exp(1);
        } else if (cleanup_enabled(Axioms.PowerIdentity) && is_one(right)) {
          left;
        } else {
          rebuild_binary(op, left, right);
        };
      } else {
        rebuild_binary(op, left, right);
      };
    | UnOp(op, inner) =>
      let inner = recurse(inner);
      cleanup_enabled(Axioms.AddIdentity) && is_zero(inner)
        ? int_exp(0) : rebuild_unary(op, inner);
    | Ap(direction, fn, arg) =>
      Exp.fresh(Ap(direction, recurse(fn), recurse(arg)))
    | Tuple(entries) => tuple_exp(List.map(recurse, entries))
    | Parens(inner) => recurse(inner)
    | Asc(inner, _) => recurse(inner)
    | Fun(pat, body, return_typ, name) =>
      Exp.fresh(Fun(pat, recurse(body), return_typ, name))
    | _ => exp
    }
  };
};

let rec cleanup_once = (~cleanup_enabled, exp) => {
  let exp = strip(exp);
  let make_diff = derivative_builder(exp);
  let recurse = cleanup_once(~cleanup_enabled);
  let changed = (capability, next) => Some((next, capability));
  switch (diff_parts(exp)) {
  | Some((expression, variable))
      when cleanup_enabled(Axioms.DerivativeBasics) =>
    let cleanup_expression = () =>
      recurse(expression)
      |> Option.map(((expression, capability)) =>
           (make_diff(expression, variable), capability)
         );
    switch (variable_name(variable)) {
    | Some(variable_name) =>
      switch (strip(expression).term) {
      | Var(name) when name == variable_name =>
        changed(Axioms.DerivativeBasics, int_exp(1))
      | Fun(_, _, _, _) => cleanup_expression()
      | _ when !depends_on(variable_name, expression) =>
        changed(Axioms.DerivativeBasics, int_exp(0))
      | _ => cleanup_expression()
      }
    | None => cleanup_expression()
    };
  | _ =>
    switch (exp.term) {
    | BinOp(op, left, right) =>
      switch (recurse(left)) {
      | Some((left, capability)) =>
        changed(capability, rebuild_binary(op, left, right))
      | None =>
        switch (recurse(right)) {
        | Some((right, capability)) =>
          changed(capability, rebuild_binary(op, left, right))
        | None =>
          if (is_operator(Operators.Plus, op)) {
            if (cleanup_enabled(Axioms.AddIdentity) && is_zero(left)) {
              changed(Axioms.AddIdentity, right);
            } else if (cleanup_enabled(Axioms.AddIdentity) && is_zero(right)) {
              changed(Axioms.AddIdentity, left);
            } else if (cleanup_enabled(Axioms.CollectLikeTerms)
                       && exp_same(left, right)) {
              changed(Axioms.CollectLikeTerms, times_exp(int_exp(2), left));
            } else if (cleanup_enabled(Axioms.CollectLikeTerms)) {
              negative_operand(right)
              |> Option.map(right =>
                   (minus_exp(left, right), Axioms.CollectLikeTerms)
                 );
            } else {
              None;
            };
          } else if (is_operator(Operators.Minus, op)) {
            if (cleanup_enabled(Axioms.AddIdentity) && is_zero(right)) {
              changed(Axioms.AddIdentity, left);
            } else if (cleanup_enabled(Axioms.AddIdentity) && is_zero(left)) {
              changed(Axioms.AddIdentity, neg_exp_for_operator(op, right));
            } else {
              None;
            };
          } else if (is_operator(Operators.Times, op)) {
            if (cleanup_enabled(Axioms.MulIdentity)
                && (is_zero(left) || is_zero(right))) {
              changed(Axioms.MulIdentity, int_exp(0));
            } else if (cleanup_enabled(Axioms.MulIdentity) && is_one(left)) {
              changed(Axioms.MulIdentity, right);
            } else if (cleanup_enabled(Axioms.MulIdentity) && is_one(right)) {
              changed(Axioms.MulIdentity, left);
            } else if (cleanup_enabled(Axioms.PowerNotation)
                       && exp_same(left, right)) {
              changed(Axioms.PowerNotation, square_power(op, left));
            } else {
              None;
            };
          } else if (is_operator(Operators.Power, op)
                     && cleanup_enabled(Axioms.PowerIdentity)) {
            if (is_zero(right)) {
              changed(Axioms.PowerIdentity, int_exp(1));
            } else if (is_one(right)) {
              changed(Axioms.PowerIdentity, left);
            } else {
              None;
            };
          } else {
            None;
          }
        }
      }
    | UnOp(op, inner) =>
      switch (recurse(inner)) {
      | Some((inner, capability)) =>
        changed(capability, rebuild_unary(op, inner))
      | None when cleanup_enabled(Axioms.AddIdentity) && is_zero(inner) =>
        changed(Axioms.AddIdentity, int_exp(0))
      | None => None
      }
    | Ap(direction, fn, arg) =>
      switch (recurse(fn)) {
      | Some((fn, capability)) =>
        changed(capability, Exp.fresh(Ap(direction, fn, arg)))
      | None =>
        recurse(arg)
        |> Option.map(((arg, capability)) =>
             (Exp.fresh(Ap(direction, fn, arg)), capability)
           )
      }
    | Tuple(entries) =>
      let rec loop = (before, remaining) =>
        switch (remaining) {
        | [] => None
        | [entry, ...rest] =>
          switch (recurse(entry)) {
          | Some((entry, capability)) =>
            changed(
              capability,
              tuple_exp(List.rev(before) @ [entry, ...rest]),
            )
          | None => loop([entry, ...before], rest)
          }
        };
      loop([], entries);
    | Parens(inner)
    | Asc(inner, _) => recurse(inner)
    | Fun(pat, body, return_typ, name) =>
      recurse(body)
      |> Option.map(((body, capability)) =>
           (Exp.fresh(Fun(pat, body, return_typ, name)), capability)
         )
    | _ => None
    }
  };
};

let is_calculus_rule_id = rule_id =>
  String.starts_with(~prefix="calc.", rule_id);
