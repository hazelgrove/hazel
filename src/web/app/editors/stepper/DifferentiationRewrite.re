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
let diff_exp = (expression, variable) =>
  app_exp("diff", tuple_exp([expression, variable]));

let function_name = exp => {
  let exp = strip(exp);
  switch (exp.term) {
  | Var(name)
  | BuiltinFun(name) => Some(name)
  | _ => None
  };
};

let diff_parts = exp => {
  let exp = strip(exp);
  switch (exp.term) {
  | Ap(Operators.Forward, fn, arg) =>
    switch (function_name(fn), strip(arg).term) {
    | (Some("diff"), Tuple([expression, variable])) =>
      Some((strip(expression), strip(variable)))
    | _ => None
    }
  | _ => None
  };
};

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
      Operators.Int(Plus) | Nat(Plus) | SInt(Plus) | Float(Plus),
    )
  | (Operators.Minus, Operators.Int(Minus) | SInt(Minus) | Float(Minus))
  | (
      Operators.Times,
      Operators.Int(Times) | Nat(Times) | SInt(Times) | Float(Times),
    )
  | (
      Operators.Divide,
      Operators.Int(Divide) | Nat(Divide) | SInt(Divide) | Float(Divide),
    )
  | (
      Operators.Power,
      Operators.Int(Power) | Nat(Power) | SInt(Power) | Float(Power),
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

let applicable_at_root = (~rule_enabled, exp) => {
  let before_exp = strip(exp);
  switch (diff_parts(before_exp)) {
  | None => []
  | Some((expression, variable)) =>
    switch (variable_name(variable)) {
    | None => []
    | Some(variable_name) =>
      let make = (rule_id, label, after_exp) =>
        enabled(~rule_enabled, rule_id)
          ? [rewrite(~rule_id, ~label, ~before_exp, ~after_exp)] : [];
      let expression = strip(expression);
      switch (expression.term) {
      | Fun(pat, body, _, _) =>
        switch (function_parameter_name(pat)) {
        | Some(bound_name) when bound_name == variable_name =>
          make(
            "calc.diff_function",
            "differentiate function body",
            diff_exp(strip(body), variable),
          )
        | _ => []
        }
      | Var(name) when name == variable_name =>
        make("calc.diff_variable", "derivative of a variable", int_exp(1))
      | Ap(Operators.Forward, fn, inner)
          when function_name(fn) == Some("sin") =>
        make(
          "calc.diff_chain_sin",
          "sine chain rule",
          times_exp(app_exp("cos", inner), diff_exp(inner, variable)),
        )
      | Ap(Operators.Forward, fn, inner)
          when function_name(fn) == Some("cos") =>
        make(
          "calc.diff_chain_cos",
          "cosine chain rule",
          times_exp(
            neg_exp(app_exp("sin", inner)),
            diff_exp(inner, variable),
          ),
        )
      | BinOp(op, left, right) when is_operator(Operators.Plus, op) =>
        make(
          "calc.diff_sum",
          "derivative sum rule",
          plus_exp(diff_exp(left, variable), diff_exp(right, variable)),
        )
      | BinOp(op, left, right) when is_operator(Operators.Minus, op) =>
        make(
          "calc.diff_difference",
          "derivative difference rule",
          minus_exp(diff_exp(left, variable), diff_exp(right, variable)),
        )
      | BinOp(op, left, right) when is_operator(Operators.Times, op) =>
        make(
          "calc.diff_product",
          "product rule",
          plus_exp(
            times_exp(diff_exp(left, variable), right),
            times_exp(left, diff_exp(right, variable)),
          ),
        )
      | BinOp(op, numerator, denominator)
          when is_operator(Operators.Divide, op) =>
        make(
          "calc.diff_quotient",
          "quotient rule (denominator nonzero)",
          divide_exp(
            minus_exp(
              times_exp(diff_exp(numerator, variable), denominator),
              times_exp(numerator, diff_exp(denominator, variable)),
            ),
            power_exp(denominator, int_exp(2)),
          ),
        )
      | BinOp(op, base, exponent) when is_operator(Operators.Power, op) =>
        switch (integer_constant(exponent)) {
        | Some(power) when power > 0 =>
          make(
            "calc.diff_power",
            "power rule",
            times_exp(
              times_exp(
                int_exp(power),
                power_exp(base, int_exp(power - 1)),
              ),
              diff_exp(base, variable),
            ),
          )
        | _ => []
        }
      | UnOp(
          Operators.Int(Operators.Minus) | SInt(Minus) | Float(Minus),
          inner,
        ) =>
        make(
          "calc.diff_negation",
          "derivative negation rule",
          neg_exp(diff_exp(inner, variable)),
        )
      | Ap(Operators.Forward, fn, inner)
          when
            depends_on(variable_name, inner)
            && !exp_same(inner, variable)
            && function_name(fn) != Some("diff") =>
        make(
          "calc.diff_chain",
          "chain rule",
          times_exp(diff_exp(fn, inner), diff_exp(inner, variable)),
        )
      | _ when !depends_on(variable_name, expression) =>
        make("calc.diff_constant", "derivative of a constant", int_exp(0))
      | _ => []
      };
    }
  };
};

let rebuild_unary = (op, inner) => Exp.fresh(UnOp(op, inner));
let rebuild_binary = (op, left, right) => Exp.fresh(BinOp(op, left, right));

let rec rewrite_first = (~rule_enabled, exp) => {
  switch (applicable_at_root(~rule_enabled, exp)) {
  | [root, ..._] => Some((root.after_exp, root))
  | [] =>
    let exp = strip(exp);
    switch (exp.term) {
    | BinOp(op, left, right) =>
      switch (rewrite_first(~rule_enabled, left)) {
      | Some((left, step)) => Some((rebuild_binary(op, left, right), step))
      | None =>
        rewrite_first(~rule_enabled, right)
        |> Option.map(((right, step)) =>
             (rebuild_binary(op, left, right), step)
           )
      }
    | UnOp(op, inner) =>
      rewrite_first(~rule_enabled, inner)
      |> Option.map(((inner, step)) => (rebuild_unary(op, inner), step))
    | Ap(direction, fn, arg) =>
      switch (rewrite_first(~rule_enabled, fn)) {
      | Some((fn, step)) => Some((Exp.fresh(Ap(direction, fn, arg)), step))
      | None =>
        rewrite_first(~rule_enabled, arg)
        |> Option.map(((arg, step)) =>
             (Exp.fresh(Ap(direction, fn, arg)), step)
           )
      }
    | Tuple(entries) =>
      let rec rewrite_entry = (before, remaining) =>
        switch (remaining) {
        | [] => None
        | [entry, ...rest] =>
          switch (rewrite_first(~rule_enabled, entry)) {
          | Some((entry, step)) =>
            Some((tuple_exp(List.rev(before) @ [entry, ...rest]), step))
          | None => rewrite_entry([entry, ...before], rest)
          }
        };
      rewrite_entry([], entries);
    | Parens(inner) =>
      rewrite_first(~rule_enabled, inner)
      |> Option.map(((inner, step)) => (Exp.fresh(Parens(inner)), step))
    | Asc(inner, typ) =>
      rewrite_first(~rule_enabled, inner)
      |> Option.map(((inner, step)) => (Exp.fresh(Asc(inner, typ)), step))
    | Projector(label, inner) =>
      rewrite_first(~rule_enabled, inner)
      |> Option.map(((inner, step)) =>
           (Exp.fresh(Projector(label, inner)), step)
         )
    | _ => None
    };
  };
};

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
  if (Option.is_some(diff_parts(exp))) {
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

let rec cleanup = (~cleanup_enabled, exp) => {
  let exp = strip(exp);
  let recurse = cleanup(~cleanup_enabled);
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
      } else {
        rebuild_binary(op, left, right);
      };
    } else if (is_operator(Operators.Minus, op)) {
      if (cleanup_enabled(Axioms.AddIdentity) && is_zero(right)) {
        left;
      } else if (cleanup_enabled(Axioms.AddIdentity) && is_zero(left)) {
        neg_exp(right);
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
      } else {
        rebuild_binary(op, left, right);
      };
    } else if (is_operator(Operators.Power, op)) {
      if (cleanup_enabled(Axioms.PowerNotation) && is_zero(right)) {
        int_exp(1);
      } else if (cleanup_enabled(Axioms.PowerNotation) && is_one(right)) {
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
  | _ => exp
  };
};

let is_calculus_rule_id = rule_id =>
  String.starts_with(~prefix="calc.", rule_id);
