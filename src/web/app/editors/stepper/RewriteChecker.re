open Util;
open Language;
open ProofTrace;

let rec take_auto_steps = (~settings, ~env, exp: Exp.t): Exp.t => {
  switch (EvaluatorStep.get_status(~settings, exp, env)) {
  | EvaluatorStep.AutoStep(step) =>
    switch (EvaluatorStep.take_step(step)) {
    | Some(next_exp) => take_auto_steps(~settings, ~env, next_exp)
    | None => exp
    }
  | AvailableSteps(_) => exp
  };
};

type affine = {
  constant: Bigint.t,
  terms: list((string, Bigint.t)),
};

type monomial = list(string);

type polynomial = list((monomial, Bigint.t));

type normal_form =
  | Evaluated(Exp.t)
  | Affine(affine)
  | Algebraic(Exp.t)
  | Polynomial(polynomial);

type normalized = {
  normal_form,
  normal_exp: Exp.t,
  rule_ids: list(string),
};

type check_result = {
  justification: string,
  group: option(Axioms.rewrite_group),
  from_normal_exp: Exp.t,
  to_normal_exp: Exp.t,
  from_trace: list(Axioms.rewrite_rule),
  to_trace: list(Axioms.rewrite_rule),
  trace: list(Axioms.rewrite_rule),
  prover_steps: list(ProofTrace.prover_step),
  exportable: bool,
};

let trace_summary_of_result = (result: check_result): ProofTrace.trace_summary => {
  justification: result.justification,
  group_name:
    result.group |> Option.map((group: Axioms.rewrite_group) => group.name),
  from_normal_exp: result.from_normal_exp,
  to_normal_exp: result.to_normal_exp,
  from_rule_ids:
    result.from_trace |> List.map((rule: Axioms.rewrite_rule) => rule.id),
  to_rule_ids:
    result.to_trace |> List.map((rule: Axioms.rewrite_rule) => rule.id),
  rule_ids: result.trace |> List.map((rule: Axioms.rewrite_rule) => rule.id),
  prover_steps: result.prover_steps,
  exportable: result.exportable,
};

let prover_step_at =
    (
      ~origin,
      ~rule_id,
      ~before_full_exp,
      ~after_full_exp,
      ~before_exp,
      ~after_exp,
      ~occurrence,
      ~detail,
    ) => {
  origin,
  rule_id,
  before_full_exp,
  after_full_exp,
  before_exp,
  after_exp,
  occurrence,
  detail: Some(detail),
};

let prover_step =
    (
      ~origin,
      ~rule_id,
      ~before_full_exp,
      ~after_full_exp,
      ~before_exp,
      ~after_exp,
      ~detail,
    ) =>
  prover_step_at(
    ~origin,
    ~rule_id,
    ~before_full_exp,
    ~after_full_exp,
    ~before_exp,
    ~after_exp,
    ~occurrence=1,
    ~detail,
  );

let trace_summary_rules_for_ids = (summary, rule_ids) =>
  switch (summary.group_name) {
  | Some(group_name) =>
    switch (Axioms.rewrite_group_by_name(group_name)) {
    | Some(group) =>
      rule_ids
      |> List.filter_map(rule_id => Axioms.rewrite_rule_by_id(group, rule_id))
    | None => []
    }
  | None => []
  };

let print_real_for_algebrite =
  fun
  | Real.Pi => "pi"
  | Real.Rational({numerator, denominator, _}) =>
    if (Bigint.equal(denominator, Bigint.one)) {
      Bigint.to_string(numerator);
    } else {
      "("
      ++ Bigint.to_string(numerator)
      ++ "/"
      ++ Bigint.to_string(denominator)
      ++ ")";
    };

let algebrite_function_name = (exp: Exp.t): option(string) => {
  let name =
    switch (exp.term) {
    | BuiltinFun(name) => Some(name)
    | _ => None
    };
  switch (name) {
  | Some(("exp" | "log" | "sqrt" | "sin" | "cos" | "tan") as name) =>
    Some(name)
  | _ => None
  };
};

let rec print_exp_for_algebrite = (~name_other, exp: Exp.t): string =>
  switch (exp.term) {
  | Atom(Int(value)) => Bigint.to_string(value)
  | Atom(Nat(value)) => Bigint.to_string(value)
  | Atom(Float(value)) => string_of_float(value)
  | Atom(Real(value)) => print_real_for_algebrite(value)
  | Atom(Bool(value)) => string_of_bool(value)
  | Ap(Forward, fn, arg) =>
    switch (algebrite_function_name(fn)) {
    | Some(name) =>
      name ++ "(" ++ print_exp_for_algebrite(~name_other, arg) ++ ")"
    | None => name_other(exp)
    }
  // We have to manually map ** (power) to ^ in Algebrite.
  | BinOp(Int(Power) | Real(Power), exp_left, exp_right) =>
    "("
    ++ print_exp_for_algebrite(~name_other, exp_left)
    ++ " ^ "
    ++ print_exp_for_algebrite(~name_other, exp_right)
    ++ ")"
  // The other operators should work fine as-is.
  | BinOp(op, exp_left, exp_right) =>
    "("
    ++ print_exp_for_algebrite(~name_other, exp_left)
    ++ " "
    ++ Operators.bin_op_to_string(op)
    ++ " "
    ++ print_exp_for_algebrite(~name_other, exp_right)
    ++ ")"
  | UnOp(Int(Minus) | Real(Minus), exp) =>
    "(" ++ "-" ++ print_exp_for_algebrite(~name_other, exp) ++ ")"
  | Parens(exp) => "(" ++ print_exp_for_algebrite(~name_other, exp) ++ ")"
  | Var(value) => value
  // TODO: think harder about weird corner cases where we'd want to ensure the types in Cast are valid
  | Asc(exp, _) => print_exp_for_algebrite(~name_other, exp)
  | _ => name_other(exp)
  };

let trace_summary_from_rules = summary =>
  trace_summary_rules_for_ids(summary, summary.from_rule_ids);

let trace_summary_to_rules = summary =>
  trace_summary_rules_for_ids(summary, summary.to_rule_ids);

let prover_hints_for_rules = (~prover, rules) =>
  rules
  |> List.filter_map((rule: Axioms.rewrite_rule) =>
       rule.prover_hints
       |> List.find_opt((hint: Axioms.prover_hint) => hint.prover == prover)
     );

let trace_summary_prover_hints = (~prover, summary) => (
  trace_summary_from_rules(summary) |> prover_hints_for_rules(~prover),
  trace_summary_to_rules(summary) |> prover_hints_for_rules(~prover),
);

type checker = {
  justification: string,
  group: option(Axioms.rewrite_group),
  normalize:
    (~settings: CoreSettings.t, ~env: Environment.t(Exp.t), Exp.t) =>
    option(normalized),
  equivalent: (normal_form, normal_form) => bool,
};

let is_zero = Bigint.equal(Bigint.zero);

let dedup = MathRewriteUtil.dedup;

let trace_rule = (primary_group, rule_id) =>
  switch (Axioms.rewrite_rule_by_id(primary_group, rule_id)) {
  | Some(rule) => Some(rule)
  | None =>
    Axioms.rewrite_groups
    |> List.find_map(group => Axioms.rewrite_rule_by_id(group, rule_id))
  };

let trace_rules = (group, rule_ids) =>
  rule_ids |> dedup |> List.filter_map(rule_id => trace_rule(group, rule_id));

let group_named_at_level = (level, name) =>
  Axioms.allowed_groups(level)
  |> List.find_opt((group: Axioms.rewrite_group) => group.name == name);

let arithmetic_group_at_level = level =>
  group_named_at_level(level, "arithmetic");

let algebra_group_at_level = level => group_named_at_level(level, "algebra");

let trigonometry_group_at_level = level =>
  group_named_at_level(level, "trigonometry");

let calculus_group_at_level = level =>
  group_named_at_level(level, "calculus");

let rec add_term = (name, coeff, terms) =>
  if (is_zero(coeff)) {
    terms;
  } else {
    switch (terms) {
    | [] => [(name, coeff)]
    | [(name', coeff'), ...rest] when name == name' =>
      let coeff'' = Bigint.(+)(coeff, coeff');
      is_zero(coeff'') ? rest : [(name, coeff''), ...rest];
    | [term, ...rest] => [term, ...add_term(name, coeff, rest)]
    };
  };

let canonicalize = (a: affine): affine => {
  let terms =
    a.terms
    |> List.fold_left(
         (terms, (name, coeff)) => add_term(name, coeff, terms),
         [],
       )
    |> List.sort(((name1, _), (name2, _)) => String.compare(name1, name2));
  {
    constant: a.constant,
    terms,
  };
};

let affine_const = constant => {
  constant,
  terms: [],
};

let affine_var = name => {
  constant: Bigint.zero,
  terms: [(name, Bigint.one)],
};

let affine_add = (a, b) =>
  canonicalize({
    constant: Bigint.(+)(a.constant, b.constant),
    terms: a.terms @ b.terms,
  });

let affine_negate = a =>
  canonicalize({
    constant: Bigint.neg(a.constant),
    terms:
      a.terms |> List.map(((name, coeff)) => (name, Bigint.neg(coeff))),
  });

let affine_sub = (a, b) => affine_add(a, affine_negate(b));

let affine_scale = (coeff, a) =>
  canonicalize({
    constant: Bigint.( * )(coeff, a.constant),
    terms:
      a.terms
      |> List.map(((name, term_coeff)) =>
           (name, Bigint.( * )(coeff, term_coeff))
         ),
  });

let affine_constant = (a: affine): option(Bigint.t) =>
  switch (a.terms) {
  | [] => Some(a.constant)
  | [_]
  | [_, ..._] => None
  };

let has_rule_id = (rule_id, rule_ids) => List.mem(rule_id, rule_ids);

type affine_normalization = {
  affine,
  rule_ids: list(string),
};

let affine_normalization = (affine, rule_ids) => {
  affine: canonicalize(affine),
  rule_ids: dedup(rule_ids),
};

let rec affine_of_exp = (exp: Exp.t): option(affine_normalization) =>
  switch (exp.term) {
  | Atom(Int(value))
  | Atom(Nat(value)) =>
    Some(affine_normalization(affine_const(value), []))
  | Atom(SInt(value)) =>
    Some(affine_normalization(affine_const(Bigint.of_int(value)), []))
  | Atom(Real(Real.Rational({numerator, denominator, _})))
      when Bigint.equal(denominator, Bigint.one) =>
    Some(affine_normalization(affine_const(numerator), []))
  | Var(name) => Some(affine_normalization(affine_var(name), []))
  | Parens(exp)
  | Asc(exp, _) => affine_of_exp(exp)
  | UnOp(Int(Minus) | SInt(Minus) | Real(Minus), exp) =>
    affine_of_exp(exp)
    |> Option.map(normalized =>
         affine_normalization(
           affine_negate(normalized.affine),
           ["arith.add_neg", ...normalized.rule_ids],
         )
       )
  | BinOp(Int(Plus) | Nat(Plus) | SInt(Plus) | Real(Plus), left, right) =>
    switch (affine_of_exp(left), affine_of_exp(right)) {
    | (Some(left), Some(right)) =>
      Some(
        affine_normalization(
          affine_add(left.affine, right.affine),
          [
            "arith.add_assoc",
            "arith.add_comm",
            "arith.const_fold",
            "arith.collect_like_terms",
            ...left.rule_ids @ right.rule_ids,
          ],
        ),
      )
    | _ => None
    }
  | BinOp(Int(Minus) | SInt(Minus) | Real(Minus), left, right) =>
    switch (affine_of_exp(left), affine_of_exp(right)) {
    | (Some(left), Some(right)) =>
      Some(
        affine_normalization(
          affine_sub(left.affine, right.affine),
          [
            "arith.add_assoc",
            "arith.add_neg",
            "arith.const_fold",
            "arith.collect_like_terms",
            ...left.rule_ids @ right.rule_ids,
          ],
        ),
      )
    | _ => None
    }
  | BinOp(
      Int(Times) | Nat(Times) | SInt(Times) | Real(Times),
      left,
      right,
    ) =>
    switch (affine_of_exp(left), affine_of_exp(right)) {
    | (Some(left), Some(right)) =>
      switch (affine_constant(left.affine), affine_constant(right.affine)) {
      | (Some(coeff), _) =>
        Some(
          affine_normalization(
            affine_scale(coeff, right.affine),
            [
              "arith.mul_const",
              "arith.const_fold",
              "arith.collect_like_terms",
              ...left.rule_ids @ right.rule_ids,
            ],
          ),
        )
      | (_, Some(coeff)) =>
        Some(
          affine_normalization(
            affine_scale(coeff, left.affine),
            [
              "arith.mul_const",
              "arith.const_fold",
              "arith.collect_like_terms",
              ...left.rule_ids @ right.rule_ids,
            ],
          ),
        )
      | (None, None) => None
      }
    | _ => None
    }
  | Atom(Float(_) | Decimal(_) | Real(_) | Bool(_) | String(_))
  | UnOp(Nat(Minus) | Float(Minus) | Bool(_), _)
  | BinOp(
      Int(
        Power | Divide | LessThan | LessThanOrEqual | GreaterThan |
        GreaterThanOrEqual,
      ) |
      Nat(
        Minus | Power | Divide | LessThan | LessThanOrEqual | GreaterThan |
        GreaterThanOrEqual,
      ) |
      SInt(
        Power | Divide | LessThan | LessThanOrEqual | GreaterThan |
        GreaterThanOrEqual,
      ) |
      Real(_) |
      Float(_) |
      Bool(_) |
      String(_) |
      Poly(_),
      _,
      _,
    )
  | Tuple(_)
  | TupleExtension(_)
  | ListLit(_)
  | ListConcat(_)
  | Cons(_)
  | TupLabel(_)
  | Dot(_)
  | Fun(_)
  | FixF(_)
  | Closure(_)
  | Ap(_)
  | TypFun(_)
  | TypAp(_)
  | Let(_)
  | Seq(_)
  | If(_)
  | Match(_)
  | Filter(_)
  | Test(_)
  | HintedTest(_)
  | Theorem(_)
  | Explore(_)
  | ProofObject(_)
  | Forall(_)
  | DynamicErrorHole(_)
  | EmptyHole
  | MultiHole(_)
  | Invalid(_)
  | Deferral(_)
  | Undefined
  | LivelitName(_)
  | Module(_)
  | ModuleExp(_)
  | TyAlias(_)
  | Use(_)
  | Projector(_)
  | DeferredAp(_)
  | BuiltinFun(_)
  | Constructor(_)
  | Label(_)
  | ExplicitNonlabel
  | DrvQuote(_) => None
  };

let normalize_affine = (~settings, ~env, exp: Exp.t): option(normal_form) => {
  exp
  |> DHExp.strip_ascriptions
  |> take_auto_steps(~settings, ~env)
  |> affine_of_exp
  |> Option.map(normalized => Affine(canonicalize(normalized.affine)));
};

let int_exp = MathRewriteUtil.int_exp;

let var_exp = name => Exp.fresh(Var(name));

let plus_exp = MathRewriteUtil.plus_exp;

let minus_exp = (left, right) =>
  Exp.fresh(BinOp(Operators.Int(Operators.Minus), left, right));

let times_exp = MathRewriteUtil.times_exp;

let negate_exp = exp =>
  Exp.fresh(UnOp(Operators.Int(Operators.Minus), exp));

let exp_of_term = ((name, coeff)) => {
  let variable = var_exp(name);
  if (Bigint.equal(coeff, Bigint.one)) {
    variable;
  } else if (Bigint.equal(coeff, Bigint.neg(Bigint.one))) {
    negate_exp(variable);
  } else if (Bigint.(<)(coeff, Bigint.zero)) {
    negate_exp(times_exp(int_exp(Bigint.abs(coeff)), variable));
  } else {
    times_exp(int_exp(coeff), variable);
  };
};

let exp_of_affine = (a: affine): Exp.t => {
  let terms = a.terms |> List.map(exp_of_term);
  let parts = is_zero(a.constant) ? terms : terms @ [int_exp(a.constant)];
  switch (parts) {
  | [] => int_exp(Bigint.zero)
  | [part] => part
  | [first, second, ...rest] =>
    List.fold_left(plus_exp, plus_exp(first, second), rest)
  };
};

let simplify_arithmetic = (~settings, ~env, exp: Exp.t): option(Exp.t) => {
  switch (normalize_affine(~settings, ~env, exp)) {
  | Some(Affine(affine)) => Some(exp_of_affine(affine))
  | _ => None
  };
};

let is_plus_op = MathRewriteUtil.is_plus_op;

let is_times_op = MathRewriteUtil.is_times_op;

let is_divide_op =
  fun
  | Operators.Int(Operators.Divide)
  | Nat(Divide)
  | SInt(Divide)
  | Real(Divide)
  | Float(Divide) => true
  | _ => false;

let is_power_op =
  fun
  | Operators.Int(Operators.Power)
  | Nat(Power)
  | SInt(Power)
  | Real(Power) => true
  | _ => false;

let strip_math_wrappers = MathRewriteUtil.strip_math_wrappers;

let rec trace_addition_terms = (exp: Exp.t): list(Exp.t) => {
  let exp = exp |> DHExp.strip_ascriptions |> strip_math_wrappers;
  switch (exp.term) {
  | BinOp(plus_op, left, right) when is_plus_op(plus_op) =>
    trace_addition_terms(left) @ trace_addition_terms(right)
  | _ => [exp]
  };
};

let trace_sum_exp = terms =>
  switch (terms) {
  | [] => int_exp(Bigint.zero)
  | [term] => term
  | [first, second, ...rest] =>
    List.fold_left(plus_exp, plus_exp(first, second), rest)
  };

let trace_int_constant = exp => {
  let exp = exp |> DHExp.strip_ascriptions |> strip_math_wrappers;
  switch (exp.term) {
  | Atom(Int(value))
  | Atom(Nat(value)) => Some(value)
  | Atom(SInt(value)) => Some(Bigint.of_int(value))
  | _ => None
  };
};

let trace_term_key = exp => {
  let exp = exp |> DHExp.strip_ascriptions |> strip_math_wrappers;
  switch (exp.term, trace_int_constant(exp)) {
  | (Var(name), _) => "0:" ++ name
  | (_, Some(value)) => "2:" ++ Bigint.to_string(value)
  | _ => "1:" ++ Exp.show(exp)
  };
};

let trace_order_terms = terms =>
  terms
  |> List.sort((left, right) =>
       String.compare(trace_term_key(left), trace_term_key(right))
     );

let trace_fold_constants = terms => {
  let (non_constants, constant) =
    terms
    |> List.fold_left(
         ((non_constants, constant), term) =>
           switch (trace_int_constant(term)) {
           | Some(value) => (non_constants, Bigint.(+)(constant, value))
           | None => (non_constants @ [term], constant)
           },
         ([], Bigint.zero),
       );
  is_zero(constant) ? non_constants : non_constants @ [int_exp(constant)];
};

let trace_step = (~origin, ~rule_id, ~before_, ~after_, ~detail) =>
  prover_step(
    ~origin,
    ~rule_id,
    ~before_full_exp=before_,
    ~after_full_exp=after_,
    ~before_exp=before_,
    ~after_exp=after_,
    ~detail,
  );

let normalizer_prover_steps = (~from_, ~to_, rule_ids) => {
  let current = ref(from_ |> DHExp.strip_ascriptions |> strip_math_wrappers);
  let add_step = (steps, rule_id, detail, after_) => {
    let before_ = current^;
    current := after_;
    steps
    @ [
      trace_step(~origin=Normalization, ~rule_id, ~before_, ~after_, ~detail),
    ];
  };
  let steps = [];
  let steps =
    has_rule_id("arith.add_assoc", rule_ids)
      ? add_step(
          steps,
          "arith.add_assoc",
          "flatten/reassociate addition",
          current^ |> trace_addition_terms |> trace_sum_exp,
        )
      : steps;
  let steps =
    has_rule_id("arith.add_comm", rule_ids)
      ? add_step(
          steps,
          "arith.add_comm",
          "order additive terms",
          current^
          |> trace_addition_terms
          |> trace_order_terms
          |> trace_sum_exp,
        )
      : steps;
  let steps =
    has_rule_id("arith.const_fold", rule_ids)
      ? add_step(
          steps,
          "arith.const_fold",
          "fold integer constants",
          current^
          |> trace_addition_terms
          |> trace_fold_constants
          |> trace_sum_exp,
        )
      : steps;
  let steps =
    has_rule_id("arith.collect_like_terms", rule_ids)
      ? add_step(
          steps,
          "arith.collect_like_terms",
          "collect matching variable terms",
          to_ |> DHExp.strip_ascriptions |> strip_math_wrappers,
        )
      : steps;
  let steps =
    has_rule_id("arith.mul_const", rule_ids)
      ? add_step(
          steps,
          "arith.mul_const",
          "represent repeated terms with a constant multiple",
          current^,
        )
      : steps;
  let algebra_rule_steps =
    [
      ("arith.add_neg", "cancel additive inverses"),
      ("alg.distribute_mul_add", "distribute multiplication over addition"),
      (
        "alg.distribute_div_add",
        "distribute division over addition or subtraction",
      ),
      ("alg.factor_common", "factor common multiplicative term"),
      ("alg.expand_polynomial", "expand polynomial product"),
      ("alg.collect_like_terms", "collect polynomial like terms"),
      ("alg.cancel_common_add", "cancel additive polynomial terms"),
    ]
    |> List.filter_map(((rule_id, detail)) =>
         has_rule_id(rule_id, rule_ids)
           ? Some(
               trace_step(
                 ~origin=Normalization,
                 ~rule_id,
                 ~before_=from_,
                 ~after_=to_,
                 ~detail,
               ),
             )
           : None
       );
  steps @ algebra_rule_steps;
};

let exp_compare = (left, right) =>
  String.compare(Exp.show(left), Exp.show(right));

let plus_exp_with_op = MathRewriteUtil.plus_exp_with_op;

let times_exp_with_op = MathRewriteUtil.times_exp_with_op;

let sorted_pair = (left, right) =>
  exp_compare(left, right) <= 0 ? (left, right) : (right, left);

let normalized_product = (times_op, left, right) => {
  let (left, right) = sorted_pair(left, right);
  times_exp_with_op(times_op, left, right);
};

let normalized_sum = (plus_op, left, right) => {
  let (left, right) = sorted_pair(left, right);
  plus_exp_with_op(plus_op, left, right);
};

let distributed_additive_exp = (add_op, left, right) =>
  switch (add_op) {
  | Operators.Int(Operators.Plus)
  | Nat(Plus)
  | SInt(Plus)
  | Real(Plus) => normalized_sum(add_op, left, right)
  | Operators.Int(Operators.Minus)
  | SInt(Minus)
  | Real(Minus) => plus_exp_with_op(add_op, left, right)
  | _ => normalized_sum(add_op, left, right)
  };

let is_minus_op =
  fun
  | Operators.Int(Operators.Minus)
  | SInt(Minus)
  | Real(Minus)
  | Float(Minus) => true
  | _ => false;

let is_int_two = exp =>
  switch (strip_math_wrappers(exp).term) {
  | Atom(Int(value))
  | Atom(Nat(value)) => Bigint.equal(value, Bigint.of_int(2))
  | Atom(SInt(value)) => value == 2
  | Atom(Real(Real.Rational({numerator, denominator, _}))) =>
    Bigint.equal(denominator, Bigint.one)
    && Bigint.equal(numerator, Bigint.of_int(2))
  | _ => false
  };

let square_exp_with_op = (power_op, exp) =>
  Exp.fresh(BinOp(power_op, exp, int_exp(Bigint.of_int(2))));

let square_exp = exp =>
  square_exp_with_op(Operators.Int(Operators.Power), exp);

let same_math_numeric_bin_op = (left, right) =>
  is_plus_op(left)
  && is_plus_op(right)
  || is_minus_op(left)
  && is_minus_op(right)
  || is_times_op(left)
  && is_times_op(right)
  || is_divide_op(left)
  && is_divide_op(right)
  || is_power_op(left)
  && is_power_op(right);

let is_numeric_minus = (op: Operators.op_un) =>
  switch (op) {
  | Operators.Int(Operators.Minus)
  | Nat(Minus)
  | SInt(Minus)
  | Real(Minus)
  | Float(Minus) => true
  | _ => false
  };

let is_real_math_builtin =
  fun
  | "sin"
  | "cos"
  | "tan"
  | "sin_real"
  | "cos_real"
  | "tan_real" => true
  | _ => false;

let rec same_math_exp = (left, right) => {
  let left = strip_math_wrappers(left);
  let right = strip_math_wrappers(right);
  if (Exp.fast_equal(left, right)) {
    true;
  } else {
    switch (left.term, right.term) {
    | (BinOp(left_op, left_a, left_b), BinOp(right_op, right_a, right_b))
        when same_math_numeric_bin_op(left_op, right_op) =>
      same_math_exp(left_a, right_a) && same_math_exp(left_b, right_b)
    | (UnOp(left_op, left_inner), UnOp(right_op, right_inner))
        when is_numeric_minus(left_op) && is_numeric_minus(right_op) =>
      same_math_exp(left_inner, right_inner)
    | (
        Ap(left_direction, left_fn, left_arg),
        Ap(right_direction, right_fn, right_arg),
      ) =>
      left_direction == right_direction
      && same_math_exp(left_fn, right_fn)
      && same_math_exp(left_arg, right_arg)
    | (Atom(Int(left)), Atom(Nat(right)))
    | (Atom(Nat(left)), Atom(Int(right))) => Bigint.equal(left, right)
    | (Atom(Int(left)), Atom(SInt(right)))
    | (Atom(Nat(left)), Atom(SInt(right))) =>
      Bigint.equal(left, Bigint.of_int(right))
    | (Atom(SInt(left)), Atom(Int(right)))
    | (Atom(SInt(left)), Atom(Nat(right))) =>
      Bigint.equal(Bigint.of_int(left), right)
    | (
        Atom(Real(Real.Rational(left))),
        Atom(Real(Real.Rational(right))),
      ) =>
      Bigint.equal(left.numerator, right.numerator)
      && Bigint.equal(left.denominator, right.denominator)
    | (
        Atom(Real(Real.Rational(real))),
        Atom(Int(integer) | Nat(integer)),
      )
    | (
        Atom(Int(integer) | Nat(integer)),
        Atom(Real(Real.Rational(real))),
      ) =>
      Bigint.equal(real.denominator, Bigint.one)
      && Bigint.equal(real.numerator, integer)
    | (BuiltinFun(left), Var(right))
    | (Var(left), BuiltinFun(right)) =>
      left == right && is_real_math_builtin(left)
    | _ => false
    };
  };
};

type rational_coeff = {
  numerator: Bigint.t,
  denominator: Bigint.t,
};

type rational_affine = {
  constant: rational_coeff,
  terms: list((Exp.t, rational_coeff)),
};

type rational_affine_piece = {
  atom: option(Exp.t),
  coeff: rational_coeff,
};

let rational_coeff = (numerator, denominator) =>
  Bigint.(<)(denominator, Bigint.zero)
    ? {
      numerator: Bigint.neg(numerator),
      denominator: Bigint.neg(denominator),
    }
    : {
      numerator,
      denominator,
    };

let rational_zero = rational_coeff(Bigint.zero, Bigint.one);
let rational_one = rational_coeff(Bigint.one, Bigint.one);

let rational_is_zero = value => Bigint.equal(value.numerator, Bigint.zero);

let rational_equal = (left, right) =>
  Bigint.equal(
    Bigint.( * )(left.numerator, right.denominator),
    Bigint.( * )(right.numerator, left.denominator),
  );

let rational_add = (left, right) =>
  rational_coeff(
    Bigint.(+)(
      Bigint.( * )(left.numerator, right.denominator),
      Bigint.( * )(right.numerator, left.denominator),
    ),
    Bigint.( * )(left.denominator, right.denominator),
  );

let rational_negate = value =>
  rational_coeff(Bigint.neg(value.numerator), value.denominator);

let rational_multiply = (left, right) =>
  rational_coeff(
    Bigint.( * )(left.numerator, right.numerator),
    Bigint.( * )(left.denominator, right.denominator),
  );

let rational_inverse = value =>
  rational_is_zero(value)
    ? None : Some(rational_coeff(value.denominator, value.numerator));

let rec rational_add_term = (term, coeff, terms) =>
  if (rational_is_zero(coeff)) {
    terms;
  } else {
    switch (terms) {
    | [] => [(term, coeff)]
    | [(candidate, candidate_coeff), ...rest]
        when same_math_exp(term, candidate) =>
      let combined = rational_add(coeff, candidate_coeff);
      rational_is_zero(combined) ? rest : [(candidate, combined), ...rest];
    | [candidate, ...rest] => [
        candidate,
        ...rational_add_term(term, coeff, rest),
      ]
    };
  };

let rational_affine_canonicalize = affine => {
  ...affine,
  terms:
    affine.terms
    |> List.fold_left(
         (terms, (term, coeff)) => rational_add_term(term, coeff, terms),
         [],
       ),
};

let rational_affine_constant = constant => {
  constant,
  terms: [],
};

let rational_affine_atom = exp => {
  constant: rational_zero,
  terms: [(strip_math_wrappers(exp), rational_one)],
};

let rational_affine_add = (left, right) =>
  rational_affine_canonicalize({
    constant: rational_add(left.constant, right.constant),
    terms: left.terms @ right.terms,
  });

let rational_affine_negate = affine =>
  rational_affine_canonicalize({
    constant: rational_negate(affine.constant),
    terms:
      affine.terms
      |> List.map(((term, coeff)) => (term, rational_negate(coeff))),
  });

let rational_affine_scale = (scalar, affine) =>
  rational_affine_canonicalize({
    constant: rational_multiply(scalar, affine.constant),
    terms:
      affine.terms
      |> List.map(((term, coeff)) =>
           (term, rational_multiply(scalar, coeff))
         ),
  });

let rational_affine_as_constant = affine =>
  switch (affine.terms) {
  | [] => Some(affine.constant)
  | [_]
  | [_, ..._] => None
  };

let rational_affine_is_scalar_target = affine =>
  switch (affine.terms) {
  | [] => true
  | [_] => rational_is_zero(affine.constant)
  | [_, ..._] => false
  };

let rec rational_affine_of_exp_with_distribution =
        (allow_distribution, exp: Exp.t): rational_affine => {
  let exp = strip_math_wrappers(exp);
  switch (exp.term) {
  | Atom(Int(value))
  | Atom(Nat(value)) =>
    rational_affine_constant(rational_coeff(value, Bigint.one))
  | Atom(SInt(value)) =>
    rational_affine_constant(
      rational_coeff(Bigint.of_int(value), Bigint.one),
    )
  | Atom(Real(Real.Rational({numerator, denominator, _}))) =>
    rational_affine_constant(rational_coeff(numerator, denominator))
  | Atom(Float(value)) when value == Float.round(value) =>
    rational_affine_constant(
      rational_coeff(Bigint.of_int(int_of_float(value)), Bigint.one),
    )
  | Parens(inner)
  | Asc(inner, _) =>
    rational_affine_of_exp_with_distribution(allow_distribution, inner)
  | UnOp(Int(Minus) | SInt(Minus) | Real(Minus) | Float(Minus), inner) =>
    rational_affine_of_exp_with_distribution(allow_distribution, inner)
    |> rational_affine_negate
  | BinOp(op, left, right) when is_plus_op(op) =>
    rational_affine_add(
      rational_affine_of_exp_with_distribution(allow_distribution, left),
      rational_affine_of_exp_with_distribution(allow_distribution, right),
    )
  | BinOp(op, left, right) when is_minus_op(op) =>
    rational_affine_add(
      rational_affine_of_exp_with_distribution(allow_distribution, left),
      rational_affine_of_exp_with_distribution(allow_distribution, right)
      |> rational_affine_negate,
    )
  | BinOp(op, left, right) when is_times_op(op) =>
    let left_affine =
      rational_affine_of_exp_with_distribution(allow_distribution, left);
    let right_affine =
      rational_affine_of_exp_with_distribution(allow_distribution, right);
    switch (
      rational_affine_as_constant(left_affine),
      rational_affine_as_constant(right_affine),
    ) {
    | (Some(scalar), _)
        when
          allow_distribution || rational_affine_is_scalar_target(right_affine) =>
      rational_affine_scale(scalar, right_affine)
    | (_, Some(scalar))
        when
          allow_distribution || rational_affine_is_scalar_target(left_affine) =>
      rational_affine_scale(scalar, left_affine)
    | (None, None) => rational_affine_atom(exp)
    | _ => rational_affine_atom(exp)
    };
  | BinOp(op, numerator, denominator) when is_divide_op(op) =>
    let denominator_affine =
      rational_affine_of_exp_with_distribution(
        allow_distribution,
        denominator,
      );
    let numerator_affine =
      rational_affine_of_exp_with_distribution(allow_distribution, numerator);
    switch (rational_affine_as_constant(denominator_affine)) {
    | Some(denominator) =>
      switch (rational_inverse(denominator)) {
      | Some(scale)
          when
            allow_distribution
            || rational_affine_is_scalar_target(numerator_affine) =>
        rational_affine_scale(scale, numerator_affine)
      | Some(_)
      | None => rational_affine_atom(exp)
      }
    | None => rational_affine_atom(exp)
    };
  | _ => rational_affine_atom(exp)
  };
};

let rational_affine_of_exp = exp =>
  rational_affine_of_exp_with_distribution(false, exp);

let rec rational_affine_terms_equal = (left_terms, right_terms) =>
  switch (left_terms, right_terms) {
  | ([], []) => true
  | (
      [(left_term, left_coeff), ...left_rest],
      [(right_term, right_coeff), ...right_rest],
    ) =>
    same_math_exp(left_term, right_term)
    && rational_equal(left_coeff, right_coeff)
    && rational_affine_terms_equal(left_rest, right_rest)
  | ([], [_, ..._])
  | ([_, ..._], []) => false
  };

let rational_affine_piece_same_atom = (left, right) =>
  switch (left.atom, right.atom) {
  | (None, None) => true
  | (Some(left), Some(right)) => same_math_exp(left, right)
  | (None, Some(_))
  | (Some(_), None) => false
  };

let rec rational_affine_append_piece = (pieces, piece) =>
  rational_is_zero(piece.coeff)
    ? pieces
    : (
      switch (pieces) {
      | [] => [piece]
      | [last] when rational_affine_piece_same_atom(last, piece) =>
        let coeff = rational_add(last.coeff, piece.coeff);
        rational_is_zero(coeff)
          ? []
          : [
            {
              ...last,
              coeff,
            },
          ];
      | [first, ...rest] => [
          first,
          ...rational_affine_append_piece(rest, piece),
        ]
      }
    );

let rational_affine_append_pieces = (left, right) =>
  right
  |> List.fold_left(
       (pieces, piece) => rational_affine_append_piece(pieces, piece),
       left,
     );

let rational_affine_scale_pieces = (scalar, pieces) =>
  pieces
  |> List.filter_map(piece => {
       let coeff = rational_multiply(scalar, piece.coeff);
       rational_is_zero(coeff)
         ? None
         : Some({
             ...piece,
             coeff,
           });
     });

let rec rational_affine_pieces_of_exp_with_distribution =
        (allow_distribution, exp: Exp.t) => {
  let exp = strip_math_wrappers(exp);
  switch (exp.term) {
  | Atom(Int(value))
  | Atom(Nat(value)) => [
      {
        atom: None,
        coeff: rational_coeff(value, Bigint.one),
      },
    ]
  | Atom(SInt(value)) => [
      {
        atom: None,
        coeff: rational_coeff(Bigint.of_int(value), Bigint.one),
      },
    ]
  | Atom(Real(Real.Rational({numerator, denominator, _}))) => [
      {
        atom: None,
        coeff: rational_coeff(numerator, denominator),
      },
    ]
  | Parens(inner)
  | Asc(inner, _) =>
    rational_affine_pieces_of_exp_with_distribution(allow_distribution, inner)
  | UnOp(Int(Minus) | SInt(Minus) | Real(Minus) | Float(Minus), inner) =>
    rational_affine_scale_pieces(
      rational_coeff(Bigint.neg(Bigint.one), Bigint.one),
      rational_affine_pieces_of_exp_with_distribution(
        allow_distribution,
        inner,
      ),
    )
  | BinOp(op, left, right) when is_plus_op(op) =>
    rational_affine_append_pieces(
      rational_affine_pieces_of_exp_with_distribution(
        allow_distribution,
        left,
      ),
      rational_affine_pieces_of_exp_with_distribution(
        allow_distribution,
        right,
      ),
    )
  | BinOp(op, left, right) when is_minus_op(op) =>
    rational_affine_append_pieces(
      rational_affine_pieces_of_exp_with_distribution(
        allow_distribution,
        left,
      ),
      rational_affine_scale_pieces(
        rational_coeff(Bigint.neg(Bigint.one), Bigint.one),
        rational_affine_pieces_of_exp_with_distribution(
          allow_distribution,
          right,
        ),
      ),
    )
  | BinOp(op, left, right) when is_times_op(op) =>
    let left_affine =
      rational_affine_of_exp_with_distribution(allow_distribution, left);
    let right_affine =
      rational_affine_of_exp_with_distribution(allow_distribution, right);
    switch (
      rational_affine_as_constant(left_affine),
      rational_affine_as_constant(right_affine),
    ) {
    | (Some(scalar), _)
        when
          allow_distribution || rational_affine_is_scalar_target(right_affine) =>
      rational_affine_scale_pieces(
        scalar,
        rational_affine_pieces_of_exp_with_distribution(
          allow_distribution,
          right,
        ),
      )
    | (_, Some(scalar))
        when
          allow_distribution || rational_affine_is_scalar_target(left_affine) =>
      rational_affine_scale_pieces(
        scalar,
        rational_affine_pieces_of_exp_with_distribution(
          allow_distribution,
          left,
        ),
      )
    | _ => [
        {
          atom: Some(exp),
          coeff: rational_one,
        },
      ]
    };
  | BinOp(op, numerator, denominator) when is_divide_op(op) =>
    let denominator_affine =
      rational_affine_of_exp_with_distribution(
        allow_distribution,
        denominator,
      );
    let numerator_affine =
      rational_affine_of_exp_with_distribution(allow_distribution, numerator);
    switch (rational_affine_as_constant(denominator_affine)) {
    | Some(denominator) =>
      switch (rational_inverse(denominator)) {
      | Some(scale)
          when
            allow_distribution
            || rational_affine_is_scalar_target(numerator_affine) =>
        rational_affine_scale_pieces(
          scale,
          rational_affine_pieces_of_exp_with_distribution(
            allow_distribution,
            numerator,
          ),
        )
      | Some(_)
      | None => [
          {
            atom: Some(exp),
            coeff: rational_one,
          },
        ]
      }
    | None => [
        {
          atom: Some(exp),
          coeff: rational_one,
        },
      ]
    };
  | _ => [
      {
        atom: Some(exp),
        coeff: rational_one,
      },
    ]
  };
};

let rational_affine_pieces_of_exp = exp =>
  rational_affine_pieces_of_exp_with_distribution(false, exp);

let rec rational_affine_pieces_equal = (left, right) =>
  switch (left, right) {
  | ([], []) => true
  | ([left, ...left_rest], [right, ...right_rest]) =>
    rational_affine_piece_same_atom(left, right)
    && rational_equal(left.coeff, right.coeff)
    && rational_affine_pieces_equal(left_rest, right_rest)
  | ([], [_, ..._])
  | ([_, ..._], []) => false
  };

let rational_affine_piece_constants = pieces =>
  pieces
  |> List.fold_left(
       (constant, piece) =>
         switch (piece.atom) {
         | None => rational_add(constant, piece.coeff)
         | Some(_) => constant
         },
       rational_zero,
     );

let rational_affine_symbolic_pieces = pieces =>
  pieces |> List.filter(piece => piece.atom |> Option.is_some);

let rational_affine_pieces_equal_with_constant_reordering = (left, right) =>
  rational_equal(
    rational_affine_piece_constants(left),
    rational_affine_piece_constants(right),
  )
  && rational_affine_pieces_equal(
       rational_affine_symbolic_pieces(left),
       rational_affine_symbolic_pieces(right),
     );

let rational_affine_normal_forms_equal_with_distribution =
    (allow_left_distribution, allow_right_distribution, left, right) => {
  let left_affine =
    rational_affine_of_exp_with_distribution(allow_left_distribution, left)
    |> rational_affine_canonicalize;
  let right_affine =
    rational_affine_of_exp_with_distribution(allow_right_distribution, right)
    |> rational_affine_canonicalize;
  rational_equal(left_affine.constant, right_affine.constant)
  && rational_affine_terms_equal(left_affine.terms, right_affine.terms);
};

let rational_affine_normal_forms_equal = (left, right) =>
  rational_affine_normal_forms_equal_with_distribution(
    false,
    false,
    left,
    right,
  );

let rational_affine_equivalent = (left, right) => {
  rational_affine_normal_forms_equal(left, right)
  && rational_affine_pieces_equal(
       rational_affine_pieces_of_exp(left),
       rational_affine_pieces_of_exp(right),
     );
};

let rational_affine_equivalent_with_constant_reordering = (left, right) =>
  rational_affine_normal_forms_equal(left, right)
  && rational_affine_pieces_equal_with_constant_reordering(
       rational_affine_pieces_of_exp(left),
       rational_affine_pieces_of_exp(right),
     );

let rational_affine_equivalent_with_capabilities =
    (
      ~allow_left_distribution,
      ~allow_right_distribution,
      ~allow_constant_reordering,
      left,
      right,
    ) =>
  rational_affine_normal_forms_equal_with_distribution(
    allow_left_distribution,
    allow_right_distribution,
    left,
    right,
  )
  && {
    let left_pieces =
      rational_affine_pieces_of_exp_with_distribution(
        allow_left_distribution,
        left,
      );
    let right_pieces =
      rational_affine_pieces_of_exp_with_distribution(
        allow_right_distribution,
        right,
      );
    allow_constant_reordering
      ? rational_affine_pieces_equal_with_constant_reordering(
          left_pieces,
          right_pieces,
        )
      : rational_affine_pieces_equal(left_pieces, right_pieces);
  };

/* Exact rational scalar arithmetic over opaque mathematical atoms (for
 * example sin(x) or f(x)).  This is deliberately guarded by the existing
 * affine-normalization profile operation: it does not distribute products,
 * reorder symbolic terms, or invoke a broader polynomial normalizer. */
let rational_affine_trace_for_profile =
    (~profile: Axioms.math_profile, from_: Exp.t, to_: Exp.t)
    : option(trace_summary) =>
  if (Axioms.normalization_rule_id_enabled_for_profile(
        profile,
        Axioms.MultiStepCheck,
        "arith.affine_normalize",
      )
      && rational_affine_equivalent_with_constant_reordering(from_, to_)
      && !same_math_exp(from_, to_)) {
    let rule_id = "arith.affine_normalize";
    Some({
      justification: "affine normalization",
      group_name: Some("arithmetic"),
      from_normal_exp: to_,
      to_normal_exp: to_,
      from_rule_ids: [rule_id],
      to_rule_ids: [],
      rule_ids: [rule_id],
      prover_steps: [
        prover_step(
          ~origin=Normalization,
          ~rule_id,
          ~before_full_exp=from_,
          ~after_full_exp=to_,
          ~before_exp=from_,
          ~after_exp=to_,
          ~detail="profile-enabled exact rational affine normalization",
        ),
      ],
      exportable: true,
    });
  } else {
    None;
  };

let rec contains_additive_shape = exp => {
  let exp = strip_math_wrappers(exp);
  switch (exp.term) {
  | BinOp(add_op, _, _) when is_plus_op(add_op) || is_minus_op(add_op) =>
    true
  | BinOp(_, left, right) =>
    contains_additive_shape(left) || contains_additive_shape(right)
  | UnOp(_, inner) => contains_additive_shape(inner)
  | Ap(_, fn, arg) =>
    contains_additive_shape(fn) || contains_additive_shape(arg)
  | _ => false
  };
};

/* Exact commutative polynomials with rational coefficients. Mathematical
 * subexpressions such as cos(2*x) are opaque atoms; only +, -, *, division by
 * a nonzero rational constant, and small natural powers are interpreted. */
type rational_monomial = list(Exp.t);
type rational_polynomial = list((rational_monomial, rational_coeff));

let rec rational_monomial_remove = (atom, factors) =>
  switch (factors) {
  | [] => None
  | [factor, ...rest] when same_math_exp(atom, factor) => Some(rest)
  | [factor, ...rest] =>
    rational_monomial_remove(atom, rest)
    |> Option.map(rest => [factor, ...rest])
  };

let rec rational_monomial_equal = (left, right) =>
  switch (left) {
  | [] => right == []
  | [factor, ...rest] =>
    switch (rational_monomial_remove(factor, right)) {
    | Some(right) => rational_monomial_equal(rest, right)
    | None => false
    }
  };

let rec rational_polynomial_add_term = (monomial, coeff, polynomial) =>
  if (rational_is_zero(coeff)) {
    polynomial;
  } else {
    switch (polynomial) {
    | [] => [(monomial, coeff)]
    | [(candidate, candidate_coeff), ...rest]
        when rational_monomial_equal(monomial, candidate) =>
      let combined = rational_add(coeff, candidate_coeff);
      rational_is_zero(combined) ? rest : [(candidate, combined), ...rest];
    | [candidate, ...rest] => [
        candidate,
        ...rational_polynomial_add_term(monomial, coeff, rest),
      ]
    };
  };

let rational_polynomial_canonicalize = polynomial =>
  polynomial
  |> List.fold_left(
       (result, (monomial, coeff)) =>
         rational_polynomial_add_term(monomial, coeff, result),
       [],
     );

let rational_polynomial_constant = coeff =>
  rational_is_zero(coeff) ? [] : [([], coeff)];

let rational_polynomial_atom = exp => [
  ([strip_math_wrappers(exp)], rational_one),
];

let rational_polynomial_add = (left, right) =>
  rational_polynomial_canonicalize(left @ right);

let rational_polynomial_scale = (scalar, polynomial) =>
  polynomial
  |> List.filter_map(((monomial, coeff)) => {
       let coeff = rational_multiply(scalar, coeff);
       rational_is_zero(coeff) ? None : Some((monomial, coeff));
     });

let rational_polynomial_negate = polynomial =>
  rational_polynomial_scale(
    rational_coeff(Bigint.neg(Bigint.one), Bigint.one),
    polynomial,
  );

let rational_polynomial_multiply = (left, right) =>
  if (List.length(left) * List.length(right) > 256) {
    None;
  } else {
    left
    |> List.fold_left(
         (result, (left_monomial, left_coeff)) =>
           right
           |> List.fold_left(
                (result, (right_monomial, right_coeff)) =>
                  rational_polynomial_add_term(
                    left_monomial @ right_monomial,
                    rational_multiply(left_coeff, right_coeff),
                    result,
                  ),
                result,
              ),
         [],
       )
    |> Option.some;
  };

let rational_polynomial_as_constant = polynomial =>
  switch (rational_polynomial_canonicalize(polynomial)) {
  | [] => Some(rational_zero)
  | [([], coeff)] => Some(coeff)
  | [_]
  | [_, ..._] => None
  };

let rec rational_polynomial_power = (base, exponent) =>
  if (exponent < 0 || exponent > 8) {
    None;
  } else if (exponent == 0) {
    Some(rational_polynomial_constant(rational_one));
  } else {
    switch (rational_polynomial_power(base, exponent - 1)) {
    | Some(powered) => rational_polynomial_multiply(base, powered)
    | None => None
    };
  };

let rational_polynomial_integer_constant = exp => {
  let exp = exp |> DHExp.strip_ascriptions |> strip_math_wrappers;
  switch (exp.term) {
  | Atom(Int(value))
  | Atom(Nat(value)) => Some(value)
  | Atom(SInt(value)) => Some(Bigint.of_int(value))
  | _ => None
  };
};

let rec rational_polynomial_of_exp = (exp: Exp.t) => {
  let exp = exp |> DHExp.strip_ascriptions |> strip_math_wrappers;
  switch (exp.term) {
  | Atom(Int(value))
  | Atom(Nat(value)) =>
    Some(rational_polynomial_constant(rational_coeff(value, Bigint.one)))
  | Atom(SInt(value)) =>
    Some(
      rational_polynomial_constant(
        rational_coeff(Bigint.of_int(value), Bigint.one),
      ),
    )
  | Parens(inner)
  | Asc(inner, _) => rational_polynomial_of_exp(inner)
  | UnOp(op, inner) when is_numeric_minus(op) =>
    rational_polynomial_of_exp(inner)
    |> Option.map(rational_polynomial_negate)
  | BinOp(op, left, right) when is_plus_op(op) || is_minus_op(op) =>
    switch (
      rational_polynomial_of_exp(left),
      rational_polynomial_of_exp(right),
    ) {
    | (Some(left), Some(right)) =>
      Some(
        rational_polynomial_add(
          left,
          is_minus_op(op) ? rational_polynomial_negate(right) : right,
        ),
      )
    | _ => None
    }
  | BinOp(op, left, right) when is_times_op(op) =>
    switch (
      rational_polynomial_of_exp(left),
      rational_polynomial_of_exp(right),
    ) {
    | (Some(left), Some(right)) =>
      rational_polynomial_multiply(left, right)
    | _ => None
    }
  | BinOp(op, numerator, denominator) when is_divide_op(op) =>
    switch (
      rational_polynomial_of_exp(numerator),
      rational_polynomial_of_exp(denominator),
    ) {
    | (Some(numerator), Some(denominator)) =>
      switch (
        rational_polynomial_as_constant(denominator)
        |> Option.bind(_, rational_inverse)
      ) {
      | Some(scale) => Some(rational_polynomial_scale(scale, numerator))
      | None => Some(rational_polynomial_atom(exp))
      }
    | _ => None
    }
  | BinOp(op, base, exponent) when is_power_op(op) =>
    switch (
      rational_polynomial_of_exp(base),
      rational_polynomial_integer_constant(exponent),
    ) {
    | (Some(base), Some(exponent)) =>
      switch (Bigint.to_int(exponent)) {
      | Some(exponent) => rational_polynomial_power(base, exponent)
      | None => Some(rational_polynomial_atom(exp))
      }
    | _ => Some(rational_polynomial_atom(exp))
    }
  | _ => Some(rational_polynomial_atom(exp))
  };
};

let rec rational_polynomial_remove_term = (monomial, coeff, polynomial) =>
  switch (polynomial) {
  | [] => None
  | [(candidate_monomial, candidate_coeff), ...rest]
      when
        rational_monomial_equal(monomial, candidate_monomial)
        && rational_equal(coeff, candidate_coeff) =>
    Some(rest)
  | [term, ...rest] =>
    rational_polynomial_remove_term(monomial, coeff, rest)
    |> Option.map(rest => [term, ...rest])
  };

let rec rational_polynomial_equal = (left, right) =>
  switch (left) {
  | [] => right == []
  | [(monomial, coeff), ...rest] =>
    switch (rational_polynomial_remove_term(monomial, coeff, right)) {
    | Some(right) => rational_polynomial_equal(rest, right)
    | None => false
    }
  };

let rational_polynomial_equivalent = (left, right) =>
  switch (
    rational_polynomial_of_exp(left),
    rational_polynomial_of_exp(right),
  ) {
  | (Some(left), Some(right)) =>
    rational_polynomial_equal(
      rational_polynomial_canonicalize(left),
      rational_polynomial_canonicalize(right),
    )
  | _ => false
  };

let rec contains_rational_division = exp => {
  let exp = strip_math_wrappers(exp);
  switch (exp.term) {
  | BinOp(op, _, denominator) when is_divide_op(op) =>
    switch (
      rational_polynomial_of_exp(denominator)
      |> Option.bind(_, rational_polynomial_as_constant)
    ) {
    | Some(value) => !rational_is_zero(value)
    | None => false
    }
  | BinOp(_, left, right) =>
    contains_rational_division(left) || contains_rational_division(right)
  | UnOp(_, inner) => contains_rational_division(inner)
  | Ap(_, fn, arg) =>
    contains_rational_division(fn) || contains_rational_division(arg)
  | _ => false
  };
};

let rec contains_rational_polynomial_expansion_shape = exp => {
  let exp = strip_math_wrappers(exp);
  switch (exp.term) {
  | BinOp(op, left, right) when is_times_op(op) =>
    contains_additive_shape(left)
    || contains_additive_shape(right)
    || contains_rational_polynomial_expansion_shape(left)
    || contains_rational_polynomial_expansion_shape(right)
  | BinOp(_, left, right) =>
    contains_rational_polynomial_expansion_shape(left)
    || contains_rational_polynomial_expansion_shape(right)
  | UnOp(_, inner) => contains_rational_polynomial_expansion_shape(inner)
  | Ap(_, fn, arg) =>
    contains_rational_polynomial_expansion_shape(fn)
    || contains_rational_polynomial_expansion_shape(arg)
  | _ => false
  };
};

let rewrites_at_one_math_occurrence = (~at_root, exp) => {
  let rec walk = exp => {
    let exp = strip_math_wrappers(exp);
    let root = at_root(exp);
    let children =
      switch (exp.term) {
      | BinOp(op, left, right) =>
        (walk(left) |> List.map(left => Exp.fresh(BinOp(op, left, right))))
        @ (
          walk(right)
          |> List.map(right => Exp.fresh(BinOp(op, left, right)))
        )
      | UnOp(op, inner) =>
        walk(inner) |> List.map(inner => Exp.fresh(UnOp(op, inner)))
      | Ap(direction, fn, arg) =>
        (walk(fn) |> List.map(fn => Exp.fresh(Ap(direction, fn, arg))))
        @ (walk(arg) |> List.map(arg => Exp.fresh(Ap(direction, fn, arg))))
      | _ => []
      };
    root @ children;
  };
  walk(exp);
};

/* Check Result may compose the focused scalar normalizer with the catalogued
 * complete-expansion macro. Opaque atoms keep this independent of any
 * particular trig function, while the explicit AC requirement prevents this
 * commutative polynomial certificate from leaking into noncommutative modes. */
let rational_polynomial_expansion_trace_for_profile =
    (~profile: Axioms.math_profile, ~stage, from_: Exp.t, to_: Exp.t) => {
  let scalar_rule_id = "arith.simplify_scalar_products";
  let expansion_rule_id = "alg.expand_polynomial";
  let plan = Axioms.stage_plan_for_profile(profile, stage);
  let manual_plan = Axioms.stage_plan_for_profile(profile, Axioms.Manual);
  let rule_enabled = rule_id =>
    Axioms.compiled_capability_enabled(plan, rule_id)
    || Axioms.compiled_capability_enabled(manual_plan, rule_id)
    || Axioms.visible_rule_enabled(profile.step_policy, rule_id);
  let division_rule_id = "alg.distribute_div_add";
  let division_rewrite =
    rule_enabled(division_rule_id)
      ? rewrites_at_one_math_occurrence(
          ~at_root=MathRewriteUtil.distribute_div_over_add_candidates,
          from_,
        )
        |> List.find_opt(candidate =>
             AlgebraIdentityRewrite.applicable_at_root(candidate)
             |> List.exists((rewrite: TrigRewrite.rewrite) =>
                  rule_enabled(rewrite.rule_id)
                  && rational_polynomial_equivalent(rewrite.after_exp, to_)
                )
           )
      : None;
  let prepared_from = division_rewrite |> Option.value(~default=from_);
  /* A named catalog identity may expose the polynomial shape that the
     expansion normalizer needs. This is a general two-phase composition over
     the enabled identity catalog, not a recognizer for a reported example. */
  let identity_rewrite =
    AlgebraIdentityRewrite.applicable_at_root(prepared_from)
    |> List.find_opt((rewrite: TrigRewrite.rewrite) =>
         rule_enabled(rewrite.rule_id)
         && rational_polynomial_equivalent(rewrite.after_exp, to_)
       );
  let identity_from =
    identity_rewrite
    |> Option.map((rewrite: TrigRewrite.rewrite) => rewrite.after_exp)
    |> Option.value(~default=prepared_from);
  let scalar_from =
    ArithmeticNormalization.simplify_scalar_products(identity_from);
  let scalar_needed = !same_math_exp(scalar_from, identity_from);
  let commutative_cleanup = [
    Axioms.AddAssoc,
    Axioms.AddComm,
    Axioms.MulAssoc,
    Axioms.MulComm,
    Axioms.MulIdentity,
    Axioms.ConstFold,
    Axioms.CollectLikeTerms,
  ];
  if (Axioms.normalization_rule_id_enabled_for_profile(
        profile,
        stage,
        expansion_rule_id,
      )
      && (
        !scalar_needed
        || Axioms.compiled_capability_enabled(plan, scalar_rule_id)
      )
      && commutative_cleanup
      |> List.for_all(capability =>
           List.mem(capability, profile.step_policy.default_cleanup)
         )
      && (
        identity_rewrite != None
        || contains_rational_polynomial_expansion_shape(identity_from)
        || contains_rational_polynomial_expansion_shape(scalar_from)
      )
      && rational_polynomial_equivalent(scalar_from, to_)
      && !same_math_exp(from_, to_)) {
    let division_steps =
      switch (division_rewrite) {
      | Some(after) => [
          prover_step(
            ~origin=Normalization,
            ~rule_id=division_rule_id,
            ~before_full_exp=from_,
            ~after_full_exp=after,
            ~before_exp=from_,
            ~after_exp=after,
            ~detail="profile-enabled quotient distribution",
          ),
        ]
      | None => []
      };
    let identity_steps =
      switch (identity_rewrite) {
      | Some(rewrite) => [
          prover_step(
            ~origin=Normalization,
            ~rule_id=rewrite.rule_id,
            ~before_full_exp=prepared_from,
            ~after_full_exp=rewrite.after_exp,
            ~before_exp=prepared_from,
            ~after_exp=rewrite.after_exp,
            ~detail="profile-enabled named polynomial identity",
          ),
        ]
      | None => []
      };
    let scalar_steps =
      scalar_needed
        ? [
          prover_step(
            ~origin=Normalization,
            ~rule_id=scalar_rule_id,
            ~before_full_exp=identity_from,
            ~after_full_exp=scalar_from,
            ~before_exp=identity_from,
            ~after_exp=scalar_from,
            ~detail="profile-enabled scalar and argument normalization",
          ),
        ]
        : [];
    let expansion_step =
      prover_step(
        ~origin=Normalization,
        ~rule_id=expansion_rule_id,
        ~before_full_exp=scalar_from,
        ~after_full_exp=to_,
        ~before_exp=scalar_from,
        ~after_exp=to_,
        ~detail="profile-enabled exact rational polynomial expansion",
      );
    let rule_ids =
      (division_rewrite != None ? [division_rule_id] : [])
      @ (
        identity_rewrite
        |> Option.map((rewrite: TrigRewrite.rewrite) => [rewrite.rule_id])
        |> Option.value(~default=[])
      )
      @ (scalar_needed ? [scalar_rule_id] : [])
      @ [expansion_rule_id];
    Some({
      justification: "rational polynomial expansion",
      group_name: Some("algebra"),
      from_normal_exp: to_,
      to_normal_exp: to_,
      from_rule_ids: rule_ids,
      to_rule_ids: [],
      rule_ids,
      prover_steps:
        division_steps @ identity_steps @ scalar_steps @ [expansion_step],
      exportable: true,
    });
  } else {
    None;
  };
};

let expand_binomial_square = exp => {
  let exp = strip_math_wrappers(exp);
  switch (exp.term) {
  | BinOp(power_op, base, exponent)
      when is_power_op(power_op) && is_int_two(exponent) =>
    switch (strip_math_wrappers(base).term) {
    | BinOp(add_op, left, right)
        when
          (is_plus_op(add_op) || is_minus_op(add_op))
          && !contains_additive_shape(left)
          && !contains_additive_shape(right) =>
      let left_square = square_exp_with_op(power_op, left);
      let middle =
        times_exp(times_exp(int_exp(Bigint.of_int(2)), left), right);
      let right_square = square_exp_with_op(power_op, right);
      Some(
        is_minus_op(add_op)
          ? plus_exp(minus_exp(left_square, middle), right_square)
          : plus_exp(plus_exp(left_square, middle), right_square),
      );
    | _ => None
    }
  | _ => None
  };
};

let conjugate_difference_of_squares =
    (plus_left, plus_right, minus_left, minus_right) =>
  if (same_math_exp(plus_left, minus_left)
      && same_math_exp(plus_right, minus_right)) {
    Some(minus_exp(square_exp(minus_left), square_exp(minus_right)));
  } else if (same_math_exp(plus_right, minus_left)
             && same_math_exp(plus_left, minus_right)) {
    Some(minus_exp(square_exp(minus_left), square_exp(minus_right)));
  } else {
    None;
  };

let multiply_conjugates = exp => {
  let exp = strip_math_wrappers(exp);
  switch (exp.term) {
  | BinOp(times_op, left, right) when is_times_op(times_op) =>
    switch (strip_math_wrappers(left).term, strip_math_wrappers(right).term) {
    | (
        BinOp(plus_op, plus_left, plus_right),
        BinOp(minus_op, minus_left, minus_right),
      )
        when is_plus_op(plus_op) && is_minus_op(minus_op) =>
      conjugate_difference_of_squares(
        plus_left,
        plus_right,
        minus_left,
        minus_right,
      )
    | (
        BinOp(minus_op, minus_left, minus_right),
        BinOp(plus_op, plus_left, plus_right),
      )
        when is_plus_op(plus_op) && is_minus_op(minus_op) =>
      conjugate_difference_of_squares(
        plus_left,
        plus_right,
        minus_left,
        minus_right,
      )
    | _ => None
    }
  | _ => None
  };
};

let rec shape_sum_terms = exp => {
  let exp = strip_math_wrappers(exp);
  switch (exp.term) {
  | BinOp(add_op, left, right) when is_plus_op(add_op) =>
    shape_sum_terms(left) @ shape_sum_terms(right)
  | _ => [exp]
  };
};

let rec shape_product_factors = exp => {
  let exp = strip_math_wrappers(exp);
  switch (exp.term) {
  | BinOp(times_op, left, right) when is_times_op(times_op) =>
    shape_product_factors(left) @ shape_product_factors(right)
  | _ => [exp]
  };
};

let square_base = exp => {
  let exp = strip_math_wrappers(exp);
  switch (exp.term) {
  | BinOp(power_op, base, exponent)
      when is_power_op(power_op) && is_int_two(exponent) =>
    Some(base)
  | _ => None
  };
};

let rec remove_matching_factor = (target, factors) =>
  switch (factors) {
  | [] => None
  | [factor, ...rest] when same_math_exp(target, factor) => Some(rest)
  | [factor, ...rest] =>
    remove_matching_factor(target, rest)
    |> Option.map(rest => [factor, ...rest])
  };

let is_factor_two = factor =>
  switch (strip_math_wrappers(factor).term) {
  | Atom(Int(value))
  | Atom(Nat(value)) => Bigint.equal(value, Bigint.of_int(2))
  | Atom(SInt(value)) => value == 2
  | _ => false
  };

let rec remove_factor_two = factors =>
  switch (factors) {
  | [] => None
  | [factor, ...rest] when is_factor_two(factor) => Some(rest)
  | [factor, ...rest] =>
    remove_factor_two(rest) |> Option.map(rest => [factor, ...rest])
  };

let middle_matches_double_product = (middle, left, right) =>
  switch (shape_product_factors(middle) |> remove_factor_two) {
  | Some(factors) =>
    switch (remove_matching_factor(left, factors)) {
    | Some(factors) =>
      switch (remove_matching_factor(right, factors)) {
      | Some([]) => true
      | _ => false
      }
    | None => false
    }
  | None => false
  };

let complete_positive_square = exp =>
  switch (shape_sum_terms(exp)) {
  | [left_square, middle, right_square] =>
    switch (square_base(left_square), square_base(right_square)) {
    | (Some(left), Some(right))
        when middle_matches_double_product(middle, left, right) =>
      Some(square_exp(plus_exp(left, right)))
    | _ => None
    }
  | _ => None
  };

let plus_op_for_minus =
  fun
  | Operators.Int(Operators.Minus) => Some(Operators.Int(Operators.Plus))
  | SInt(Minus) => Some(SInt(Plus))
  | _ => None;

let distribute_factor_over_additive_candidates = (times_op, factor, additive) => {
  let additive = strip_math_wrappers(additive);
  switch (additive.term) {
  | BinOp(add_op, _, _) when is_plus_op(add_op) => [
      shape_sum_terms(additive)
      |> List.map(term => times_exp_with_op(times_op, factor, term))
      |> trace_sum_exp,
    ]
  | BinOp(add_op, add_left, add_right) when is_minus_op(add_op) =>
    let left_product = times_exp_with_op(times_op, factor, add_left);
    let right_product = times_exp_with_op(times_op, factor, add_right);
    let subtraction =
      distributed_additive_exp(add_op, left_product, right_product);
    switch (plus_op_for_minus(add_op)) {
    | Some(plus_op) =>
      let negative_right_product =
        times_exp_with_op(times_op, factor, negate_exp(add_right));
      [
        subtraction,
        plus_exp_with_op(plus_op, left_product, negative_right_product),
      ];
    | None => [subtraction]
    };
  | _ => []
  };
};

let distribute_factor_over_additive = (times_op, factor, additive) =>
  distribute_factor_over_additive_candidates(times_op, factor, additive)
  |> ListUtil.hd_opt;

let distribute_additive_over_factor_candidates = (times_op, additive, factor) => {
  let additive = strip_math_wrappers(additive);
  switch (additive.term) {
  | BinOp(add_op, _, _) when is_plus_op(add_op) => [
      shape_sum_terms(additive)
      |> List.map(term => times_exp_with_op(times_op, term, factor))
      |> trace_sum_exp,
    ]
  | BinOp(add_op, add_left, add_right) when is_minus_op(add_op) =>
    let left_product = times_exp_with_op(times_op, add_left, factor);
    let right_product = times_exp_with_op(times_op, add_right, factor);
    let subtraction =
      distributed_additive_exp(add_op, left_product, right_product);
    switch (plus_op_for_minus(add_op)) {
    | Some(plus_op) =>
      let negative_right_product =
        times_exp_with_op(times_op, negate_exp(add_right), factor);
      [
        subtraction,
        plus_exp_with_op(plus_op, left_product, negative_right_product),
      ];
    | None => [subtraction]
    };
  | _ => []
  };
};

let distribute_additive_over_factor = (times_op, additive, factor) =>
  distribute_additive_over_factor_candidates(times_op, additive, factor)
  |> ListUtil.hd_opt;

let distribute_factor_over_quotient_numerator = (times_op, factor, quotient) => {
  let quotient = strip_math_wrappers(quotient);
  switch (quotient.term) {
  | BinOp(divide_op, numerator, denominator) when is_divide_op(divide_op) =>
    switch (strip_math_wrappers(numerator).term) {
    | BinOp(add_op, add_left, add_right)
        when is_plus_op(add_op) || is_minus_op(add_op) =>
      let left_product = times_exp_with_op(times_op, factor, add_left);
      let right_product = times_exp_with_op(times_op, factor, add_right);
      let left_quotient =
        Exp.fresh(BinOp(divide_op, left_product, denominator));
      let right_quotient =
        Exp.fresh(BinOp(divide_op, right_product, denominator));
      Some(distributed_additive_exp(add_op, left_quotient, right_quotient));
    | _ => None
    }
  | _ => None
  };
};

let distribute_mul_over_add = (exp: Exp.t): option(Exp.t) => {
  let exp = strip_math_wrappers(exp);
  switch (exp.term) {
  | BinOp(times_op, left, right) when is_times_op(times_op) =>
    let left = strip_math_wrappers(left);
    let right = strip_math_wrappers(right);
    switch (
      distribute_factor_over_additive(times_op, left, right),
      distribute_additive_over_factor(times_op, left, right),
      distribute_factor_over_quotient_numerator(times_op, left, right),
      distribute_factor_over_quotient_numerator(times_op, right, left),
    ) {
    | (Some(expanded), _, _, _)
    | (_, Some(expanded), _, _)
    | (_, _, Some(expanded), _)
    | (_, _, _, Some(expanded)) => Some(expanded)
    | _ => None
    };
  | _ => None
  };
};

let distribute_mul_over_add_candidates = (exp: Exp.t): list(Exp.t) => {
  let exp = strip_math_wrappers(exp);
  switch (exp.term) {
  | BinOp(times_op, left, right) when is_times_op(times_op) =>
    let left = strip_math_wrappers(left);
    let right = strip_math_wrappers(right);
    distribute_factor_over_additive_candidates(times_op, left, right)
    @ distribute_additive_over_factor_candidates(times_op, left, right)
    @ (
      distribute_factor_over_quotient_numerator(times_op, left, right)
      |> Option.to_list
    )
    @ (
      distribute_factor_over_quotient_numerator(times_op, right, left)
      |> Option.to_list
    );
  | _ => []
  };
};

let distribute_div_over_add_candidates = MathRewriteUtil.distribute_div_over_add_candidates;

let factors_of_product = MathRewriteUtil.factors_of_product;

let shared_factor_normal_exp =
    (plus_op, left_product, right_product): option(Exp.t) => {
  switch (
    factors_of_product(left_product),
    factors_of_product(right_product),
  ) {
  | (Some((left_times_op, a, b)), Some((right_times_op, c, d)))
      when left_times_op == right_times_op =>
    let candidates =
      [
        Exp.fast_equal(a, c) ? Some((a, b, d)) : None,
        Exp.fast_equal(a, d) ? Some((a, b, c)) : None,
        Exp.fast_equal(b, c) ? Some((b, a, d)) : None,
        Exp.fast_equal(b, d) ? Some((b, a, c)) : None,
      ]
      |> List.filter_map(x => x);
    candidates
    |> ListUtil.hd_opt
    |> Option.map(((common, other_left, other_right)) => {
         let left_exp = normalized_product(left_times_op, common, other_left);
         let right_exp =
           normalized_product(left_times_op, common, other_right);
         normalized_sum(plus_op, left_exp, right_exp);
       });
  | _ => None
  };
};

let normalize_common_factor_sum = (exp: Exp.t): option(Exp.t) => {
  let exp = strip_math_wrappers(exp);
  switch (exp.term) {
  | BinOp(plus_op, left, right) when is_plus_op(plus_op) =>
    shared_factor_normal_exp(
      plus_op,
      strip_math_wrappers(left),
      strip_math_wrappers(right),
    )
  | _ => None
  };
};

let exp_same = MathRewriteUtil.exp_same;

let rec sum_terms = (exp: Exp.t): list(Exp.t) => {
  let exp = strip_math_wrappers(exp);
  switch (exp.term) {
  | BinOp(plus_op, left, right) when is_plus_op(plus_op) =>
    sum_terms(left) @ sum_terms(right)
  | _ => [exp]
  };
};

let rec signed_sum_terms = (exp: Exp.t): list((int, Exp.t)) => {
  let exp = strip_math_wrappers(exp);
  switch (exp.term) {
  | BinOp(plus_op, left, right) when is_plus_op(plus_op) =>
    signed_sum_terms(left) @ signed_sum_terms(right)
  | BinOp(Int(Minus) | SInt(Minus), left, right) =>
    signed_sum_terms(left)
    @ (signed_sum_terms(right) |> List.map(((sign, exp)) => (- sign, exp)))
  | UnOp(Int(Minus) | SInt(Minus), exp) =>
    signed_sum_terms(exp) |> List.map(((sign, exp)) => (- sign, exp))
  | _ => [(1, exp)]
  };
};

let rec product_factors = (exp: Exp.t): list(Exp.t) => {
  let exp = strip_math_wrappers(exp);
  switch (exp.term) {
  | BinOp(times_op, left, right) when is_times_op(times_op) =>
    product_factors(left) @ product_factors(right)
  | _ => [exp]
  };
};

let cleanup_has = (capability, cleanup) =>
  cleanup
  |> List.exists((candidate: Axioms.cleanup_capability) =>
       candidate == capability
     );

let integer_power_parts = (exp: Exp.t) => {
  let exp = strip_math_wrappers(exp);
  switch (exp.term) {
  | BinOp(power_op, base, exponent) when is_power_op(power_op) =>
    let exponent =
      switch (strip_math_wrappers(exponent).term) {
      | Atom(Int(value))
      | Atom(Nat(value)) => Bigint.to_int(value)
      | Atom(SInt(value)) => Some(value)
      | _ => None
      };
    switch (exponent) {
    | Some(exponent) when exponent >= 2 => Some((base, exponent))
    | Some(_)
    | None => None
    };
  | _ => None
  };
};

/* Compare enabled power notation through the ordinary product structure. */
let rec product_factors_under_cleanup = (cleanup, exp: Exp.t): list(Exp.t) => {
  let exp = strip_math_wrappers(exp);
  switch (integer_power_parts(exp)) {
  | Some((base, exponent)) when cleanup_has(Axioms.PowerNotation, cleanup) =>
    let factors = product_factors_under_cleanup(cleanup, base);
    let rec repeat = (remaining, repeated) =>
      remaining == 0 ? repeated : repeat(remaining - 1, repeated @ factors);
    repeat(exponent, []);
  | Some(_)
  | None =>
    switch (exp.term) {
    | BinOp(times_op, left, right) when is_times_op(times_op) =>
      product_factors_under_cleanup(cleanup, left)
      @ product_factors_under_cleanup(cleanup, right)
    | _ => [exp]
    }
  };
};

let rec exp_same_up_to_cleanup = (cleanup, left, right) => {
  let left = strip_math_wrappers(left);
  let right = strip_math_wrappers(right);
  let cleanup_without_power =
    cleanup
    |> List.filter((capability: Axioms.cleanup_capability) =>
         capability != Axioms.PowerNotation
       );

  let ordered_equal = (left_items, right_items) =>
    List.length(left_items) == List.length(right_items)
    && List.for_all2(
         (left_item, right_item) =>
           exp_same_up_to_cleanup(cleanup, left_item, right_item),
         left_items,
         right_items,
       );

  let rec remove_first_matching = (target, candidates) =>
    switch (candidates) {
    | [] => None
    | [candidate, ...rest]
        when exp_same_up_to_cleanup(cleanup, target, candidate) =>
      Some(rest)
    | [candidate, ...rest] =>
      remove_first_matching(target, rest)
      |> Option.map(rest => [candidate, ...rest])
    };

  let rec unordered_equal = (left_items, right_items) =>
    switch (left_items) {
    | [] => right_items == []
    | [left_item, ...left_rest] =>
      switch (remove_first_matching(left_item, right_items)) {
      | Some(right_rest) => unordered_equal(left_rest, right_rest)
      | None => false
      }
    };

  let repeated_product = (exp: Exp.t) => {
    let factors = product_factors(exp);
    switch (factors) {
    | [first, second, ...rest]
        when
          [second, ...rest]
          |> List.for_all(factor =>
               exp_same_up_to_cleanup(cleanup_without_power, first, factor)
             ) =>
      Some((first, List.length(factors)))
    | [_, _, ..._] => None
    | []
    | [_] => None
    };
  };

  let same_up_to_power_notation =
    switch (integer_power_parts(left), repeated_product(right)) {
    | (Some((left_base, left_exponent)), Some((right_base, right_exponent))) =>
      left_exponent == right_exponent
      && exp_same_up_to_cleanup(cleanup_without_power, left_base, right_base)
    | _ =>
      switch (repeated_product(left), integer_power_parts(right)) {
      | (
          Some((left_base, left_exponent)),
          Some((right_base, right_exponent)),
        ) =>
        left_exponent == right_exponent
        && exp_same_up_to_cleanup(
             cleanup_without_power,
             left_base,
             right_base,
           )
      | _ => false
      }
    };

  if (cleanup_has(Axioms.PowerNotation, cleanup) && same_up_to_power_notation) {
    true;
  } else if (cleanup_has(Axioms.AddAssoc, cleanup)) {
    let left_terms = sum_terms(left);
    let right_terms = sum_terms(right);
    if (List.length(left_terms) > 1 || List.length(right_terms) > 1) {
      cleanup_has(Axioms.AddComm, cleanup)
        ? unordered_equal(left_terms, right_terms)
        : ordered_equal(left_terms, right_terms);
    } else if (cleanup_has(Axioms.MulAssoc, cleanup)) {
      let left_factors = product_factors_under_cleanup(cleanup, left);
      let right_factors = product_factors_under_cleanup(cleanup, right);
      if (List.length(left_factors) > 1 || List.length(right_factors) > 1) {
        cleanup_has(Axioms.MulComm, cleanup)
          ? unordered_equal(left_factors, right_factors)
          : ordered_equal(left_factors, right_factors);
      } else {
        exp_same(left, right);
      };
    } else {
      exp_same(left, right);
    };
  } else if (cleanup_has(Axioms.MulAssoc, cleanup)) {
    let left_factors = product_factors_under_cleanup(cleanup, left);
    let right_factors = product_factors_under_cleanup(cleanup, right);
    if (List.length(left_factors) > 1 || List.length(right_factors) > 1) {
      cleanup_has(Axioms.MulComm, cleanup)
        ? unordered_equal(left_factors, right_factors)
        : ordered_equal(left_factors, right_factors);
    } else {
      exp_same(left, right);
    };
  } else {
    exp_same(left, right);
  };
};

let exp_same_up_to_add_assoc = (left, right) =>
  exp_same_up_to_cleanup([Axioms.AddAssoc], left, right);

let rec remove_first_exp = (target, factors) =>
  switch (factors) {
  | [] => None
  | [factor, ...rest] when exp_same(target, factor) => Some(rest)
  | [factor, ...rest] =>
    remove_first_exp(target, rest) |> Option.map(rest => [factor, ...rest])
  };

let rec shared_factors = (left, right) =>
  switch (left) {
  | [] => []
  | [factor, ...rest] =>
    switch (remove_first_exp(factor, right)) {
    | Some(right_rest) => [factor, ...shared_factors(rest, right_rest)]
    | None => shared_factors(rest, right)
    }
  };

let common_product_factors = terms =>
  switch (terms) {
  | []
  | [_] => []
  | [first, ...rest] =>
    rest
    |> List.fold_left(
         (common, term) => shared_factors(common, product_factors(term)),
         product_factors(first),
       )
  };

let has_common_factor_sum = exp => {
  let terms =
    signed_sum_terms(exp)
    |> List.map(((_, exp)) => strip_math_wrappers(exp));
  List.length(terms) > 1 && common_product_factors(terms) != [];
};

let exp_has_sum = exp => sum_terms(exp) |> List.length > 1;

let product_has_sum_factor = exp =>
  product_factors(exp) |> List.exists(exp_has_sum);

let rec remove_matching_signed_term = ((sign, exp), terms) =>
  switch (terms) {
  | [] => None
  | [(other_sign, other_exp), ...rest]
      when sign + other_sign == 0 && exp_same(exp, other_exp) =>
    Some(rest)
  | [term, ...rest] =>
    remove_matching_signed_term((sign, exp), rest)
    |> Option.map(rest => [term, ...rest])
  };

let rec cancel_one_signed_pair = terms =>
  switch (terms) {
  | [] => None
  | [term, ...rest] =>
    switch (remove_matching_signed_term(term, rest)) {
    | Some(rest_without_match) => Some(rest_without_match)
    | None =>
      cancel_one_signed_pair(rest) |> Option.map(rest => [term, ...rest])
    }
  };

let exp_of_signed_terms = terms =>
  switch (terms) {
  | [] => int_exp(Bigint.zero)
  | [(sign, exp), ...rest] =>
    let term_exp = ((sign, exp)) => sign < 0 ? negate_exp(exp) : exp;
    rest
    |> List.fold_left(
         (acc, term) => plus_exp(acc, term_exp(term)),
         term_exp((sign, exp)),
       );
  };

let cancel_common_add_exp = exp =>
  switch (cancel_one_signed_pair(signed_sum_terms(exp))) {
  | Some(remaining) => Some(exp_of_signed_terms(remaining))
  | None => None
  };

let rec compare_string_lists = (left, right) =>
  switch (left, right) {
  | ([], []) => 0
  | ([], [_])
  | ([], [_, ..._]) => (-1)
  | ([_], [])
  | ([_, ..._], []) => 1
  | ([left_head, ...left_tail], [right_head, ...right_tail]) =>
    switch (String.compare(left_head, right_head)) {
    | 0 => compare_string_lists(left_tail, right_tail)
    | result => result
    }
  };

let monomial_compare = (left: monomial, right: monomial) => {
  let degree_order = compare(List.length(right), List.length(left));
  degree_order == 0 ? compare_string_lists(left, right) : degree_order;
};

let monomial_mul = (left: monomial, right: monomial): monomial =>
  left @ right |> List.sort(String.compare);

let rec add_polynomial_term = (monomial, coeff, polynomial) =>
  if (is_zero(coeff)) {
    polynomial;
  } else {
    switch (polynomial) {
    | [] => [(monomial, coeff)]
    | [(monomial', coeff'), ...rest] when monomial == monomial' =>
      let coeff'' = Bigint.(+)(coeff, coeff');
      is_zero(coeff'') ? rest : [(monomial, coeff''), ...rest];
    | [term, ...rest] => [
        term,
        ...add_polynomial_term(monomial, coeff, rest),
      ]
    };
  };

let polynomial_canonicalize = (polynomial: polynomial): polynomial =>
  polynomial
  |> List.fold_left(
       (acc, (monomial, coeff)) =>
         add_polynomial_term(monomial, coeff, acc),
       [],
     )
  |> List.sort(((left, _), (right, _)) => monomial_compare(left, right));

let polynomial_const = value => is_zero(value) ? [] : [([], value)];

let polynomial_var = name => [([name], Bigint.one)];

let polynomial_add = (left, right) => polynomial_canonicalize(left @ right);

let polynomial_negate = polynomial =>
  polynomial
  |> List.map(((monomial, coeff)) => (monomial, Bigint.neg(coeff)))
  |> polynomial_canonicalize;

let polynomial_sub = (left, right) =>
  polynomial_add(left, polynomial_negate(right));

let polynomial_equal = (left, right) => {
  let left = polynomial_canonicalize(left);
  let right = polynomial_canonicalize(right);
  List.length(left) == List.length(right)
  && List.for_all2(
       ((left_monomial, left_coeff), (right_monomial, right_coeff)) =>
         left_monomial == right_monomial
         && Bigint.equal(left_coeff, right_coeff),
       left,
       right,
     );
};

let polynomial_has_like_terms = (left, right) =>
  left
  |> List.exists(((left_monomial, _)) =>
       right
       |> List.exists(((right_monomial, _)) =>
            left_monomial == right_monomial
          )
     );

let polynomial_has_cancelling_terms = (left, right) =>
  left
  |> List.exists(((left_monomial, left_coeff)) =>
       right
       |> List.exists(((right_monomial, right_coeff)) =>
            left_monomial == right_monomial
            && is_zero(Bigint.(+)(left_coeff, right_coeff))
          )
     );

let polynomial_collection_rule_ids = (left, right) => {
  let like_terms = polynomial_has_like_terms(left, right);
  let cancelling_terms = polynomial_has_cancelling_terms(left, right);
  (like_terms ? ["alg.collect_like_terms"] : [])
  @ (cancelling_terms ? ["alg.cancel_common_add"] : []);
};

let polynomial_mul = (left, right) =>
  left
  |> List.fold_left(
       (acc, (left_monomial, left_coeff)) =>
         right
         |> List.fold_left(
              (acc, (right_monomial, right_coeff)) =>
                add_polynomial_term(
                  monomial_mul(left_monomial, right_monomial),
                  Bigint.( * )(left_coeff, right_coeff),
                  acc,
                ),
              acc,
            ),
       [],
     )
  |> polynomial_canonicalize;

let polynomial_multiplication_folds_constants = (left, right) => {
  let is_unit = coefficient =>
    Bigint.equal(Bigint.abs(coefficient), Bigint.one);
  left
  |> List.exists(((_, left_coefficient)) =>
       !is_unit(left_coefficient)
       && right
       |> List.exists(((_, right_coefficient)) =>
            !is_unit(right_coefficient)
          )
     );
};

let rec polynomial_power = (base, exponent) =>
  if (exponent < 0) {
    None;
  } else if (exponent == 0) {
    Some(polynomial_const(Bigint.one));
  } else if (exponent == 1) {
    Some(base);
  } else {
    switch (polynomial_power(base, exponent - 1)) {
    | Some(powered) => Some(polynomial_mul(base, powered))
    | None => None
    };
  };

type polynomial_normalization = {
  polynomial,
  rule_ids: list(string),
};

let polynomial_normalization = (polynomial, rule_ids) => {
  polynomial: polynomial_canonicalize(polynomial),
  rule_ids: dedup(rule_ids),
};

let polynomial_term_count = polynomial =>
  polynomial_canonicalize(polynomial) |> List.length;

let polynomial_int_constant = exp => {
  let exp = exp |> DHExp.strip_ascriptions |> strip_math_wrappers;
  switch (exp.term) {
  | Atom(Int(value))
  | Atom(Nat(value)) => Some(value)
  | Atom(SInt(value)) => Some(Bigint.of_int(value))
  | Atom(Real(Real.Rational({numerator, denominator, _})))
      when Bigint.equal(denominator, Bigint.one) =>
    Some(numerator)
  | _ => None
  };
};

let rec polynomial_of_exp = (exp: Exp.t): option(polynomial_normalization) => {
  let exp = exp |> DHExp.strip_ascriptions |> strip_math_wrappers;
  switch (exp.term) {
  | Atom(Int(value))
  | Atom(Nat(value)) =>
    Some(polynomial_normalization(polynomial_const(value), []))
  | Atom(SInt(value)) =>
    Some(
      polynomial_normalization(polynomial_const(Bigint.of_int(value)), []),
    )
  | Atom(Real(Real.Rational({numerator, denominator, _})))
      when Bigint.equal(denominator, Bigint.one) =>
    Some(polynomial_normalization(polynomial_const(numerator), []))
  | Var(name) => Some(polynomial_normalization(polynomial_var(name), []))
  | Parens(exp)
  | Asc(exp, _) => polynomial_of_exp(exp)
  | UnOp(Int(Minus) | SInt(Minus) | Real(Minus), exp) =>
    polynomial_of_exp(exp)
    |> Option.map(normalized =>
         polynomial_normalization(
           polynomial_negate(normalized.polynomial),
           ["alg.collect_like_terms", ...normalized.rule_ids],
         )
       )
  | BinOp(Int(Plus) | Nat(Plus) | SInt(Plus) | Real(Plus), left, right) =>
    switch (polynomial_of_exp(left), polynomial_of_exp(right)) {
    | (Some(left), Some(right)) =>
      let rule_ids =
        polynomial_collection_rule_ids(left.polynomial, right.polynomial)
        @ left.rule_ids
        @ right.rule_ids;
      Some(
        polynomial_normalization(
          polynomial_add(left.polynomial, right.polynomial),
          rule_ids,
        ),
      );
    | _ => None
    }
  | BinOp(Int(Minus) | SInt(Minus) | Real(Minus), left, right) =>
    switch (polynomial_of_exp(left), polynomial_of_exp(right)) {
    | (Some(left), Some(right)) =>
      let negated_right = polynomial_negate(right.polynomial);
      let rule_ids =
        polynomial_collection_rule_ids(left.polynomial, negated_right)
        @ left.rule_ids
        @ right.rule_ids;
      Some(
        polynomial_normalization(
          polynomial_add(left.polynomial, negated_right),
          rule_ids,
        ),
      );
    | _ => None
    }
  | BinOp(
      Int(Times) | Nat(Times) | SInt(Times) | Real(Times),
      left,
      right,
    ) =>
    switch (polynomial_of_exp(left), polynomial_of_exp(right)) {
    | (Some(left), Some(right)) =>
      let rule_ids =
        (
          polynomial_term_count(left.polynomial) > 1
          || polynomial_term_count(right.polynomial) > 1
            ? [
              "alg.expand_polynomial",
              "alg.distribute_mul_add",
              "alg.collect_like_terms",
              ...left.rule_ids @ right.rule_ids,
            ]
            : left.rule_ids @ right.rule_ids
        )
        @ (
          polynomial_multiplication_folds_constants(
            left.polynomial,
            right.polynomial,
          )
            ? ["arith.const_fold"] : []
        );
      Some(
        polynomial_normalization(
          polynomial_mul(left.polynomial, right.polynomial),
          rule_ids,
        ),
      );
    | _ => None
    }
  | BinOp(
      Int(Power) | Nat(Power) | SInt(Power) | Real(Power),
      base,
      exponent,
    ) =>
    switch (polynomial_of_exp(base), polynomial_int_constant(exponent)) {
    | (Some(base), Some(exponent)) =>
      switch (Bigint.to_int(exponent)) {
      | Some(exponent) =>
        polynomial_power(base.polynomial, exponent)
        |> Option.map(polynomial =>
             polynomial_normalization(
               polynomial,
               exponent > 1
                 ? [
                   "alg.expand_polynomial",
                   "alg.collect_like_terms",
                   ...base.rule_ids,
                 ]
                 : base.rule_ids,
             )
           )
      | None => None
      }
    | _ => None
    }
  | Atom(Float(_) | Decimal(_) | Real(_) | Bool(_) | String(_))
  | UnOp(Nat(Minus) | Float(Minus) | Bool(_), _)
  | BinOp(
      Int(
        Divide | LessThan | LessThanOrEqual | GreaterThan | GreaterThanOrEqual,
      ) |
      Nat(
        Minus | Divide | LessThan | LessThanOrEqual | GreaterThan |
        GreaterThanOrEqual,
      ) |
      SInt(
        Divide | LessThan | LessThanOrEqual | GreaterThan | GreaterThanOrEqual,
      ) |
      Real(_) |
      Float(_) |
      Bool(_) |
      String(_) |
      Poly(_),
      _,
      _,
    )
  | Tuple(_)
  | TupleExtension(_)
  | ListLit(_)
  | ListConcat(_)
  | Cons(_)
  | TupLabel(_)
  | Dot(_)
  | Fun(_)
  | FixF(_)
  | Closure(_)
  | Ap(_)
  | TypFun(_)
  | TypAp(_)
  | Let(_)
  | Seq(_)
  | If(_)
  | Match(_)
  | Filter(_)
  | Test(_)
  | HintedTest(_)
  | Theorem(_)
  | Explore(_)
  | ProofObject(_)
  | Forall(_)
  | DynamicErrorHole(_)
  | EmptyHole
  | MultiHole(_)
  | Invalid(_)
  | Deferral(_)
  | Undefined
  | LivelitName(_)
  | Module(_)
  | ModuleExp(_)
  | TyAlias(_)
  | Use(_)
  | Projector(_)
  | DeferredAp(_)
  | BuiltinFun(_)
  | Constructor(_)
  | Label(_)
  | ExplicitNonlabel
  | DrvQuote(_) => None
  };
};

let polynomial_exp_has_multiple_terms = exp =>
  switch (polynomial_of_exp(exp)) {
  | Some(normalized) => polynomial_term_count(normalized.polynomial) > 1
  | None => false
  };

let exp_of_monomial = (monomial: monomial): Exp.t =>
  switch (monomial) {
  | [] => int_exp(Bigint.one)
  | [name] => var_exp(name)
  | [first, second, ...rest] =>
    rest
    |> List.fold_left(
         (acc, name) => times_exp(acc, var_exp(name)),
         times_exp(var_exp(first), var_exp(second)),
       )
  };

let exp_of_polynomial_term = ((monomial, coeff)) => {
  switch (monomial, Bigint.compare(coeff, Bigint.zero)) {
  | ([], _) => int_exp(coeff)
  | (_, 0) => int_exp(Bigint.zero)
  | (_, _) =>
    let variable_part = exp_of_monomial(monomial);
    if (Bigint.equal(coeff, Bigint.one)) {
      variable_part;
    } else if (Bigint.equal(coeff, Bigint.neg(Bigint.one))) {
      negate_exp(variable_part);
    } else if (Bigint.(<)(coeff, Bigint.zero)) {
      negate_exp(times_exp(int_exp(Bigint.abs(coeff)), variable_part));
    } else {
      times_exp(int_exp(coeff), variable_part);
    };
  };
};

let exp_of_polynomial = (polynomial: polynomial): Exp.t => {
  let terms = polynomial |> polynomial_canonicalize;
  switch (terms) {
  | [] => int_exp(Bigint.zero)
  | [first, ...rest] =>
    rest
    |> List.fold_left(
         (acc, (monomial, coefficient) as term) =>
           if (Bigint.(<)(coefficient, Bigint.zero)) {
             minus_exp(
               acc,
               exp_of_polynomial_term((monomial, Bigint.abs(coefficient))),
             );
           } else {
             plus_exp(acc, exp_of_polynomial_term(term));
           },
         exp_of_polynomial_term(first),
       )
  };
};

let normalize_polynomial =
    (~settings as _: CoreSettings.t, ~env as _: Environment.t(Exp.t), exp) => {
  let exp = exp |> DHExp.strip_ascriptions |> strip_math_wrappers;
  let shape_rule_ids =
    (
      switch (normalize_common_factor_sum(exp)) {
      | Some(_) => ["alg.factor_common"]
      | None => []
      }
    )
    @ (has_common_factor_sum(exp) ? ["alg.factor_common"] : [])
    @ (
      switch (cancel_common_add_exp(exp)) {
      | Some(_) => ["alg.cancel_common_add"]
      | None => []
      }
    );
  exp
  |> polynomial_of_exp
  |> Option.map(normalized => {
       let polynomial = polynomial_canonicalize(normalized.polynomial);
       {
         normal_form: Polynomial(polynomial),
         normal_exp: exp_of_polynomial(polynomial),
         rule_ids: dedup(shape_rule_ids @ normalized.rule_ids),
       };
     });
};

let power_has_sum_base = exp => {
  let exp = exp |> DHExp.strip_ascriptions |> strip_math_wrappers;
  switch (exp.term) {
  | BinOp(power_op, base, exponent)
      when is_power_op(power_op) && polynomial_int_constant(exponent) != None =>
    polynomial_exp_has_multiple_terms(base)
  | _ => false
  };
};

let polynomial_expansion_target = exp => {
  let exp = exp |> DHExp.strip_ascriptions |> strip_math_wrappers;
  let product_has_polynomial_sum_factor =
    product_factors(exp) |> List.exists(polynomial_exp_has_multiple_terms);
  if (product_has_polynomial_sum_factor || power_has_sum_base(exp)) {
    switch (polynomial_of_exp(exp)) {
    | Some(normalized)
        when List.mem("alg.expand_polynomial", normalized.rule_ids) =>
      Some((
        normalized.polynomial |> polynomial_canonicalize |> exp_of_polynomial,
        normalized.rule_ids,
      ))
    | _ => None
    };
  } else {
    None;
  };
};

let normalize_algebra_shape = exp =>
  switch (complete_positive_square(exp)) {
  | Some(factored) => Some((factored, ["alg.factor_common"]))
  | None =>
    switch (expand_binomial_square(exp)) {
    | Some(expanded) => Some((expanded, ["alg.expand_polynomial"]))
    | None =>
      switch (multiply_conjugates(exp)) {
      | Some(expanded) => Some((expanded, ["alg.expand_polynomial"]))
      | None =>
        switch (distribute_div_over_add_candidates(exp)) {
        | [expanded, ..._] => Some((expanded, ["alg.distribute_div_add"]))
        | [] =>
          switch (distribute_mul_over_add(exp)) {
          | Some(expanded) => Some((expanded, ["alg.distribute_mul_add"]))
          | None =>
            switch (normalize_common_factor_sum(exp)) {
            | Some(expanded) =>
              Some((
                expanded,
                ["alg.factor_common", "alg.distribute_mul_add"],
              ))
            | None => polynomial_expansion_target(exp)
            }
          }
        }
      }
    }
  };

let normalize_algebra_distribution =
    (~settings as _: CoreSettings.t, ~env as _: Environment.t(Exp.t), exp) => {
  let exp = exp |> DHExp.strip_ascriptions |> strip_math_wrappers;
  switch (normalize_algebra_shape(exp)) {
  | Some((expanded, rule_ids)) =>
    Some({
      normal_form: Algebraic(expanded),
      normal_exp: expanded,
      rule_ids,
    })
  | None =>
    Some({
      normal_form: Algebraic(exp),
      normal_exp: exp,
      rule_ids: [],
    })
  };
};

let normalize_affine_with_trace =
    (~settings, ~env, exp: Exp.t): option(normalized) => {
  exp
  |> DHExp.strip_ascriptions
  |> take_auto_steps(~settings, ~env)
  |> affine_of_exp
  |> Option.map(normalized => {
       let affine = canonicalize(normalized.affine);
       {
         normal_form: Affine(affine),
         normal_exp: exp_of_affine(affine),
         rule_ids: normalized.rule_ids,
       };
     });
};

let polynomial_equivalent_exps = (left, right) =>
  switch (polynomial_of_exp(left), polynomial_of_exp(right)) {
  | (Some(left), Some(right)) =>
    polynomial_equal(left.polynomial, right.polynomial)
  | _ => false
  };

let rec has_explicit_like_terms = exp => {
  let exp = exp |> DHExp.strip_ascriptions |> strip_math_wrappers;
  let operands_have_like_terms = (left, right, ~negate_right) =>
    switch (polynomial_of_exp(left), polynomial_of_exp(right)) {
    | (Some(left_normalized), Some(right_normalized)) =>
      let right_polynomial =
        negate_right
          ? polynomial_negate(right_normalized.polynomial)
          : right_normalized.polynomial;
      polynomial_has_like_terms(left_normalized.polynomial, right_polynomial)
      || has_explicit_like_terms(left)
      || has_explicit_like_terms(right);
    | _ => false
    };
  switch (exp.term) {
  | BinOp(plus_op, left, right) when is_plus_op(plus_op) =>
    operands_have_like_terms(left, right, ~negate_right=false)
  | BinOp(Int(Minus) | SInt(Minus), left, right) =>
    operands_have_like_terms(left, right, ~negate_right=true)
  | Parens(inner)
  | Asc(inner, _) => has_explicit_like_terms(inner)
  | _ => false
  };
};

let rec has_distributive_expansion_shape = exp => {
  let exp = exp |> DHExp.strip_ascriptions |> strip_math_wrappers;
  switch (exp.term) {
  | BinOp(times_op, left, right) when is_times_op(times_op) =>
    polynomial_exp_has_multiple_terms(left)
    || polynomial_exp_has_multiple_terms(right)
    || has_distributive_expansion_shape(left)
    || has_distributive_expansion_shape(right)
  | BinOp(power_op, base, exponent) when is_power_op(power_op) =>
    power_has_sum_base(exp)
    || has_distributive_expansion_shape(base)
    || has_distributive_expansion_shape(exponent)
  | BinOp(_, left, right) =>
    has_distributive_expansion_shape(left)
    || has_distributive_expansion_shape(right)
  | UnOp(_, inner)
  | Parens(inner)
  | Asc(inner, _) => has_distributive_expansion_shape(inner)
  | _ => false
  };
};

let polynomial_collection_signature = exp =>
  polynomial_of_exp(exp)
  |> Option.map(normalized =>
       (
         normalized.polynomial,
         has_explicit_like_terms(exp),
         has_distributive_expansion_shape(exp),
       )
     );

/* Collection is a visible algebra operation, not an unrestricted polynomial
 * normalizer.  Accept equivalent polynomials only when one displayed shape
 * actually contains like terms and neither side requires distribution. */
let check_single_collect_like_terms = (group, from_, to_) => {
  switch (
    polynomial_collection_signature(from_),
    polynomial_collection_signature(to_),
  ) {
  | (
      Some((from_polynomial, from_collects, from_expands)),
      Some((to_polynomial, to_collects, to_expands)),
    )
      when
        polynomial_equal(from_polynomial, to_polynomial)
        && from_collects != to_collects
        && !from_expands
        && !to_expands =>
    let trace = trace_rules(group, ["alg.collect_like_terms"]);
    Some({
      justification: "collect like terms",
      group: Some(group),
      from_normal_exp: to_,
      to_normal_exp: to_,
      from_trace: trace,
      to_trace: [],
      trace,
      prover_steps: [
        prover_step(
          ~origin=ManualRewrite,
          ~rule_id="alg.collect_like_terms",
          ~before_full_exp=from_,
          ~after_full_exp=to_,
          ~before_exp=from_,
          ~after_exp=to_,
          ~detail="collect polynomial like terms",
        ),
      ],
      exportable: true,
    });
  | _ => None
  };
};

let polynomial_cleanup_requirements_allowed = (profile, exp) =>
  switch (polynomial_of_exp(exp)) {
  | None => false
  | Some(normalized) =>
    normalized.rule_ids
    |> List.for_all(rule_id =>
         switch (Axioms.cleanup_capability_for_id(rule_id)) {
         | Some(capability) =>
           List.mem(capability, profile.Axioms.step_policy.default_cleanup)
         | None => true
         }
       )
  };

/*
 * Expand multiplication over additive operands without combining any of the
 * resulting terms.  This is deliberately separate from polynomial_of_exp:
 * the polynomial normalizer combines equal monomials, while a profile with
 * CollectLikeTerms disabled must still be able to perform distribution.
 */
let rec fully_distributed_signed_terms = (exp: Exp.t): list((int, Exp.t)) => {
  let exp = exp |> DHExp.strip_ascriptions |> strip_math_wrappers;
  switch (exp.term) {
  | BinOp(plus_op, left, right) when is_plus_op(plus_op) =>
    fully_distributed_signed_terms(left)
    @ fully_distributed_signed_terms(right)
  | BinOp(Int(Minus) | SInt(Minus), left, right) =>
    fully_distributed_signed_terms(left)
    @ (
      fully_distributed_signed_terms(right)
      |> List.map(((sign, term)) => (- sign, term))
    )
  | UnOp(Int(Minus) | SInt(Minus), inner) =>
    fully_distributed_signed_terms(inner)
    |> List.map(((sign, term)) => (- sign, term))
  | BinOp(times_op, left, right) when is_times_op(times_op) =>
    fully_distributed_signed_terms(left)
    |> List.concat_map(((left_sign, left_term)) =>
         fully_distributed_signed_terms(right)
         |> List.map(((right_sign, right_term)) =>
              (
                left_sign * right_sign,
                times_exp_with_op(times_op, left_term, right_term),
              )
            )
       )
  | _ => [(1, exp)]
  };
};

let product_term_same_under_cleanup = (cleanup, left, right) =>
  if (exp_same_up_to_cleanup(cleanup, left, right)) {
    true;
  } else if (!cleanup_has(Axioms.MulAssoc, cleanup)) {
    false;
  } else {
    let cleanup_enabled = capability => cleanup_has(capability, cleanup);
    let normalize_identities = exp =>
      DifferentiationRewrite.cleanup(~cleanup_enabled, exp);
    let normalize_factors = exp => {
      let factors =
        product_factors_under_cleanup(cleanup, normalize_identities(exp));
      if (cleanup_has(Axioms.ConstFold, cleanup)) {
        let (constant, constant_count, symbolic) =
          factors
          |> List.fold_left(
               ((constant, count, symbolic), factor) =>
                 switch (polynomial_int_constant(factor)) {
                 | Some(value) => (
                     Bigint.( * )(constant, value),
                     count + 1,
                     symbolic,
                   )
                 | None => (constant, count, symbolic @ [factor])
                 },
               (Bigint.one, 0, []),
             );
        if (constant_count == 0) {
          symbolic;
        } else if (Bigint.equal(constant, Bigint.one)
                   && symbolic != []
                   && cleanup_has(Axioms.MulIdentity, cleanup)) {
          symbolic;
        } else {
          [int_exp(constant), ...symbolic];
        };
      } else {
        factors;
      };
    };
    let left_factors = normalize_factors(left);
    let right_factors = normalize_factors(right);
    let rec remove_matching = (target, candidates) =>
      switch (candidates) {
      | [] => None
      | [candidate, ...rest]
          when exp_same_up_to_cleanup(cleanup, target, candidate) =>
        Some(rest)
      | [candidate, ...rest] =>
        remove_matching(target, rest)
        |> Option.map(rest => [candidate, ...rest])
      };
    let rec unordered_equal = (left, right) =>
      switch (left) {
      | [] => right == []
      | [head, ...rest] =>
        switch (remove_matching(head, right)) {
        | Some(right) => unordered_equal(rest, right)
        | None => false
        }
      };
    List.length(left_factors) == List.length(right_factors)
    && (
      cleanup_has(Axioms.MulComm, cleanup)
        ? unordered_equal(left_factors, right_factors)
        : List.for_all2(
            (left, right) => exp_same_up_to_cleanup(cleanup, left, right),
            left_factors,
            right_factors,
          )
    );
  };

let uncollected_full_distribution_matches = (profile, from_, to_) => {
  let cleanup = profile.Axioms.step_policy.default_cleanup;
  if (!cleanup_has(Axioms.AddAssoc, cleanup)) {
    false;
  } else {
    let from_terms = fully_distributed_signed_terms(from_);
    let to_terms = signed_sum_terms(to_);
    let term_same = ((left_sign, left), (right_sign, right)) =>
      left_sign == right_sign
      && product_term_same_under_cleanup(cleanup, left, right);
    let rec remove_matching = (target, candidates) =>
      switch (candidates) {
      | [] => None
      | [candidate, ...rest] when term_same(target, candidate) => Some(rest)
      | [candidate, ...rest] =>
        remove_matching(target, rest)
        |> Option.map(rest => [candidate, ...rest])
      };
    let rec unordered_equal = (left, right) =>
      switch (left) {
      | [] => right == []
      | [head, ...rest] =>
        switch (remove_matching(head, right)) {
        | Some(right) => unordered_equal(rest, right)
        | None => false
        }
      };
    List.length(from_terms) > 1
    && List.length(from_terms) == List.length(to_terms)
    && (
      cleanup_has(Axioms.AddComm, cleanup)
        ? unordered_equal(from_terms, to_terms)
        : List.for_all2(term_same, from_terms, to_terms)
    );
  };
};

let polynomial_normal_exp = exp =>
  exp
  |> polynomial_of_exp
  |> Option.map(normalized =>
       normalized.polynomial |> polynomial_canonicalize |> exp_of_polynomial
     )
  |> Option.value(~default=exp);

let simplify_algebra = (~settings as _: CoreSettings.t, ~env as _, exp) =>
  exp
  |> DHExp.strip_ascriptions
  |> strip_math_wrappers
  |> polynomial_of_exp
  |> Option.map(normalized =>
       normalized.polynomial |> polynomial_canonicalize |> exp_of_polynomial
     );

let simplify_at_level = (~level, ~settings, ~env, exp) =>
  switch (level) {
  | Axioms.Arithmetic => simplify_arithmetic(~settings, ~env, exp)
  | Axioms.Algebra =>
    switch (simplify_algebra(~settings, ~env, exp)) {
    | Some(simplified) => Some(simplified)
    | None => simplify_arithmetic(~settings, ~env, exp)
    }
  | Axioms.Trigonometry =>
    switch (simplify_algebra(~settings, ~env, exp)) {
    | Some(simplified) => Some(simplified)
    | None => simplify_arithmetic(~settings, ~env, exp)
    }
  | Axioms.FunctionsAndLists
  | Axioms.Calculus =>
    switch (simplify_algebra(~settings, ~env, exp)) {
    | Some(simplified) => Some(simplified)
    | None => simplify_arithmetic(~settings, ~env, exp)
    }
  };

let simplify_for_profile =
    (~profile: Axioms.math_profile, ~settings, ~env, exp) => {
  switch (profile.level) {
  | Axioms.Calculus =>
    let rule_enabled = rule_id =>
      Axioms.visible_rule_enabled(profile.step_policy, rule_id);
    let normalized =
      DifferentiationRewrite.normalize(~rule_enabled, ~fuel=128, exp);
    let cleanup_enabled = capability =>
      List.mem(capability, profile.step_policy.default_cleanup);
    let cleaned =
      DifferentiationRewrite.cleanup(~cleanup_enabled, normalized.exp);
    switch (normalized.steps) {
    | [] when TrigRewrite.exp_same(cleaned, exp) =>
      simplify_at_level(~level=profile.level, ~settings, ~env, exp)
    | []
    | [_, ..._] => Some(cleaned)
    };
  | _ => simplify_at_level(~level=profile.level, ~settings, ~env, exp)
  };
};

type local_rewrite = {
  before_exp: Exp.t,
  after_exp: Exp.t,
  occurrence: int,
  detail: string,
};

let whole_local_rewrite = (~detail, from_, to_) => {
  before_exp: from_,
  after_exp: to_,
  occurrence: 1,
  detail,
};

let find_distributed_additive_term =
    (cleanup, from_, to_): option(local_rewrite) => {
  let rec loop = (prefix, occurrence, terms) =>
    switch (terms) {
    | [] => None
    | [term, ...rest] =>
      switch (
        distribute_mul_over_add_candidates(term)
        |> List.find_opt(after_term => {
             let candidate =
               trace_sum_exp(prefix @ sum_terms(after_term) @ rest);
             exp_same_up_to_cleanup(cleanup, candidate, to_);
           })
      ) {
      | Some(after_term) =>
        Some({
          before_exp: term,
          after_exp: after_term,
          occurrence,
          detail:
            "single distribution at additive term "
            ++ string_of_int(occurrence),
        })
      | None => loop(prefix @ [term], occurrence + 1, rest)
      }
    };
  loop([], 1, sum_terms(from_));
};

let has_single_distributed_additive_term = (cleanup, from_, to_) =>
  find_distributed_additive_term(cleanup, from_, to_) |> Option.is_some;

let local_rewrite_for_algebra = (rule_ids, from_, to_) =>
  if (has_rule_id("alg.distribute_mul_add", rule_ids)) {
    switch (find_distributed_additive_term(Axioms.ac_cleanup, from_, to_)) {
    | Some(local) => local
    | None =>
      whole_local_rewrite(
        ~detail="single algebra rule over whole expression",
        from_,
        to_,
      )
    };
  } else if (has_rule_id("alg.distribute_div_add", rule_ids)) {
    switch (distribute_div_over_add_candidates(from_)) {
    | [after_exp, ..._] => {
        before_exp: from_,
        after_exp,
        occurrence: 1,
        detail: "single division distribution",
      }
    | [] =>
      whole_local_rewrite(
        ~detail="single algebra rule over whole expression",
        from_,
        to_,
      )
    };
  } else {
    whole_local_rewrite(
      ~detail="single algebra rule over whole expression",
      from_,
      to_,
    );
  };

let single_algebra_result = (group, rule_ids, from_, to_) => {
  let trace = trace_rules(group, rule_ids);
  let local = local_rewrite_for_algebra(rule_ids, from_, to_);
  {
    justification: "algebra one step",
    group: Some(group),
    from_normal_exp: polynomial_normal_exp(from_),
    to_normal_exp: polynomial_normal_exp(to_),
    from_trace: trace,
    to_trace: [],
    trace,
    prover_steps:
      rule_ids
      |> List.map(rule_id =>
           prover_step_at(
             ~origin=ManualRewrite,
             ~rule_id,
             ~before_full_exp=from_,
             ~after_full_exp=to_,
             ~before_exp=local.before_exp,
             ~after_exp=local.after_exp,
             ~occurrence=local.occurrence,
             ~detail=local.detail,
           )
         ),
    exportable: true,
  };
};

let check_single_cancel_common_add = (group, from_, to_) =>
  switch (cancel_common_add_exp(from_)) {
  | Some(cancelled) when polynomial_equivalent_exps(cancelled, to_) =>
    Some(
      single_algebra_result(group, ["alg.cancel_common_add"], from_, to_),
    )
  | _ => None
  };

let distribution_policy_allows_simplification =
  fun
  | Axioms.StrictDistributedForm => false
  | DistributionMaySimplify => true;

let check_single_division_distribution = (group, profile, from_, to_) =>
  if (!
        Axioms.visible_rule_enabled(
          profile.Axioms.step_policy,
          "alg.distribute_div_add",
        )) {
    None;
  } else {
    distribute_div_over_add_candidates(from_)
    |> List.find_opt(distributed => TrigRewrite.exp_same(distributed, to_))
    |> Option.map(_ =>
         single_algebra_result(group, ["alg.distribute_div_add"], from_, to_)
       );
  };

let check_single_distribution_or_expansion = (group, profile, from_, to_) => {
  let policy = profile.Axioms.one_step_policy;
  let expansion_enabled =
    Axioms.compiled_capability_enabled(
      Axioms.stage_plan_for_profile(profile, Axioms.Manual),
      "alg.expand_polynomial",
    );
  let is_named_algebra_identity =
    AlgebraIdentityRewrite.applicable_at_root(from_)
    |> List.exists((rewrite: TrigRewrite.rewrite) =>
         TrigRewrite.exp_same(rewrite.after_exp, to_)
       );
  if (is_named_algebra_identity) {
    None;
  } else if (!
               Axioms.visible_rule_enabled(
                 profile.step_policy,
                 "alg.distribute_mul_add",
               )) {
    None;
  } else {
    let distribution_cleanup =
      Axioms.cleanup_for_visible_rule(
        profile.step_policy,
        "alg.distribute_mul_add",
      );
    let sum_factor_count =
      product_factors(from_)
      |> List.filter(polynomial_exp_has_multiple_terms)
      |> List.length;
    let may_simplify_distribution =
      distribution_policy_allows_simplification(
        policy.Axioms.distribution_step_policy,
      );
    if ((exp_has_sum(to_) || polynomial_exp_has_multiple_terms(to_))
        && has_single_distributed_additive_term(
             distribution_cleanup,
             from_,
             to_,
           )) {
      Some(
        single_algebra_result(group, ["alg.distribute_mul_add"], from_, to_),
      );
    } else if (may_simplify_distribution
               && product_has_sum_factor(from_)
               && polynomial_exp_has_multiple_terms(to_)
               && polynomial_equivalent_exps(from_, to_)) {
      Some(
        single_algebra_result(group, ["alg.distribute_mul_add"], from_, to_),
      );
    } else if (sum_factor_count > 1
               && expansion_enabled
               && uncollected_full_distribution_matches(profile, from_, to_)
               && polynomial_equivalent_exps(from_, to_)) {
      Some(
        single_algebra_result(
          group,
          ["alg.expand_polynomial", "alg.distribute_mul_add"],
          from_,
          to_,
        ),
      );
    } else if (sum_factor_count > 1
               && expansion_enabled
               && List.mem(
                    Axioms.CollectLikeTerms,
                    profile.Axioms.step_policy.default_cleanup,
                  )
               && List.length(signed_sum_terms(to_))
               < List.length(fully_distributed_signed_terms(from_))
               && polynomial_cleanup_requirements_allowed(profile, from_)
               && polynomial_exp_has_multiple_terms(to_)
               && polynomial_equivalent_exps(from_, to_)) {
      Some(
        single_algebra_result(
          group,
          ["alg.expand_polynomial", "alg.distribute_mul_add"],
          from_,
          to_,
        ),
      );
    } else {
      None;
    };
  };
};

let check_single_factor_common = (group, from_, to_) => {
  let syntactic_factor =
    switch (normalize_common_factor_sum(from_)) {
    | Some(factored) => exp_same(factored, to_)
    | None => false
    };
  if (has_common_factor_sum(from_)
      && product_has_sum_factor(to_)
      && (syntactic_factor || polynomial_equivalent_exps(from_, to_))) {
    Some(single_algebra_result(group, ["alg.factor_common"], from_, to_));
  } else {
    None;
  };
};

let check_single_algebra_identity = (group, profile, from_, to_) =>
  AlgebraIdentityRewrite.applicable_at_root(from_)
  |> List.find_map((rewrite: TrigRewrite.rewrite) =>
       if (Axioms.visible_rule_enabled(
             profile.Axioms.step_policy,
             rewrite.rule_id,
           )
           && TrigRewrite.exp_same(rewrite.after_exp, to_)) {
         let trace = trace_rules(group, [rewrite.rule_id]);
         Some({
           justification: "algebra identity one step",
           group: Some(group),
           from_normal_exp: from_,
           to_normal_exp: to_,
           from_trace: trace,
           to_trace: [],
           trace,
           prover_steps: [
             prover_step(
               ~origin=ManualRewrite,
               ~rule_id=rewrite.rule_id,
               ~before_full_exp=from_,
               ~after_full_exp=to_,
               ~before_exp=rewrite.before_exp,
               ~after_exp=rewrite.after_exp,
               ~detail="single algebra identity",
             ),
           ],
           exportable: true,
         });
       } else {
         None;
       }
     );

let normalize_by_evaluation =
    (~settings as _: CoreSettings.t, ~env, exp: Exp.t): option(normalized) => {
  switch (Evaluator.evaluate_and_limit(~env, ~step_limit=1000, exp)) {
  | LimitedCompleted((value, _)) =>
    let normal_exp = value |> DHExp.strip_ascriptions;
    Some({
      normal_form: Evaluated(normal_exp),
      normal_exp,
      rule_ids: [],
    });
  | StepLimitExceeded => None
  | exception _ => None
  };
};

let affine_checker_at_level = level => {
  justification: "arithmetic",
  group: arithmetic_group_at_level(level),
  normalize: normalize_affine_with_trace,
  equivalent: (left, right) =>
    switch (left, right) {
    | (Affine(left), Affine(right)) => left == right
    | _ => false
    },
};

let affine_checker = affine_checker_at_level(Axioms.Arithmetic);

let algebra_distribution_checker_at_level = level => {
  let group = algebra_group_at_level(level);
  {
    justification: "algebra",
    group,
    normalize:
      switch (group) {
      | Some(_) => normalize_polynomial
      | None => ((~settings as _, ~env as _, _) => None)
      },
    equivalent: (left, right) =>
      switch (left, right) {
      | (Polynomial(left), Polynomial(right)) => left == right
      | (Algebraic(left), Algebraic(right)) => Exp.fast_equal(left, right)
      | _ => false
      },
  };
};

let evaluation_checker = {
  justification: "same evaluated result",
  group: None,
  normalize: normalize_by_evaluation,
  equivalent: (left, right) =>
    switch (left, right) {
    | (Evaluated(left), Evaluated(right)) =>
      Equality.equality(
        Equality.{
          ...Equality.semantic_settings,
          env1: Some(Environment.empty),
          env2: Some(Environment.empty),
          ignore_ascriptions: true,
        },
      ).
        exp(
        left,
        right,
      )
    | _ => false
    },
};

let check_with = (~settings, ~env, from_: Exp.t, to_: Exp.t, checker) => {
  switch (
    checker.normalize(~settings, ~env, from_),
    checker.normalize(~settings, ~env, to_),
  ) {
  | (Some(from_normal), Some(to_normal))
      when checker.equivalent(from_normal.normal_form, to_normal.normal_form) =>
    let (from_trace, to_trace) =
      switch (checker.group) {
      | Some(group) => (
          trace_rules(group, from_normal.rule_ids),
          trace_rules(group, to_normal.rule_ids),
        )
      | None => ([], [])
      };
    let trace = dedup(from_trace @ to_trace);
    switch (checker.group, trace) {
    | (Some(_), []) => None
    | _ =>
      Some({
        justification: checker.justification,
        group: checker.group,
        from_normal_exp: from_normal.normal_exp,
        to_normal_exp: to_normal.normal_exp,
        from_trace,
        to_trace,
        trace,
        prover_steps:
          normalizer_prover_steps(
            ~from_,
            ~to_,
            trace |> List.map((rule: Axioms.rewrite_rule) => rule.id),
          ),
        exportable: trace != [],
      })
    };
  | _ => None
  };
};

let exp_matches = (~env, left, right) =>
  Equality.equality(
    Equality.{
      ...Equality.semantic_settings,
      env1: Some(env),
      env2: Some(env),
      ignore_ascriptions: true,
    },
  ).
    exp(
    left,
    right,
  );

let single_eval_step_results = (~settings, ~env, from_: Exp.t) => {
  let take_and_justify = (step: EvaluatorStep.step) => {
    switch (EvaluatorStep.take_step(step)) {
    | Some(next_exp) =>
      let kind = EvaluatorStep.get_step_kind(step);
      let justification = Transition.stepper_justification(kind);
      let final_exp = take_auto_steps(~settings, ~env, next_exp);
      Some((justification, final_exp));
    | None => None
    };
  };
  let rec get_next_exps = (exp: Exp.t) => {
    switch (EvaluatorStep.get_status(~settings, exp, env)) {
    | EvaluatorStep.AutoStep(step) =>
      switch (EvaluatorStep.take_step(step)) {
      | Some(next_exp) => get_next_exps(next_exp)
      | None => []
      }
    | AvailableSteps(steps) => List.filter_map(take_and_justify, steps)
    };
  };
  get_next_exps(from_);
};

let check_single_eval_step_result =
    (~settings, ~env, from_: Exp.t, to_: Exp.t): option(check_result) => {
  single_eval_step_results(~settings, ~env, from_)
  |> List.find_map(((justification, normal_exp)) =>
       if (exp_matches(~env, normal_exp, to_)) {
         Some({
           justification,
           group: None,
           from_normal_exp: normal_exp,
           to_normal_exp: to_ |> DHExp.strip_ascriptions,
           from_trace: [],
           to_trace: [],
           trace: [],
           prover_steps: [],
           exportable: false,
         });
       } else {
         None;
       }
     );
};

let check_single_eval_step_trace =
    (~settings, ~env, from_: Exp.t, to_: Exp.t): option(trace_summary) =>
  check_single_eval_step_result(~settings, ~env, from_, to_)
  |> Option.map(trace_summary_of_result);

let rec flatten_addition =
        (exp: Exp.t): option((Operators.op_bin, list(Exp.t))) => {
  let exp = exp |> DHExp.strip_ascriptions |> strip_math_wrappers;
  switch (exp.term) {
  | BinOp(plus_op, left, right) when is_plus_op(plus_op) =>
    let left_terms =
      switch (flatten_addition(left)) {
      | Some((_, terms)) => terms
      | None => [left |> DHExp.strip_ascriptions |> strip_math_wrappers]
      };
    let right_terms =
      switch (flatten_addition(right)) {
      | Some((_, terms)) => terms
      | None => [right |> DHExp.strip_ascriptions |> strip_math_wrappers]
      };
    Some((plus_op, left_terms @ right_terms));
  | _ => None
  };
};

let exactly_one_adjacent_swap = (left, right) => {
  let rec loop = (prefix_equal, left, right) =>
    switch (left, right) {
    | ([], []) => false
    | ([l1, l2, ...left_rest], [r1, r2, ...right_rest])
        when
          prefix_equal
          && Exp.fast_equal(l1, r2)
          && Exp.fast_equal(l2, r1)
          && List.length(left_rest) == List.length(right_rest)
          && List.for_all2(Exp.fast_equal, left_rest, right_rest) =>
      true
    | ([l, ...left_rest], [r, ...right_rest]) when Exp.fast_equal(l, r) =>
      loop(prefix_equal, left_rest, right_rest)
    | _ => false
    };
  List.length(left) == List.length(right) && loop(true, left, right);
};

let int_constant = MathRewriteUtil.int_constant;

let rec exp_is_integer_arithmetic = exp => {
  let exp = exp |> DHExp.strip_ascriptions |> strip_math_wrappers;
  switch (exp.term) {
  | Atom(Int(_))
  | Atom(Nat(_))
  | Atom(SInt(_)) => true
  | BinOp(op, left, right)
      when is_plus_op(op) || is_minus_op(op) || is_times_op(op) =>
    exp_is_integer_arithmetic(left) && exp_is_integer_arithmetic(right)
  | _ => false
  };
};

let exactly_one_integer_distribution = (profile, from_, to_) => {
  let policy = profile.Axioms.one_step_policy;
  let distribution_cleanup =
    Axioms.cleanup_for_visible_rule(profile.step_policy, "arith.mul_const");
  Axioms.visible_rule_enabled(profile.step_policy, "arith.mul_const")
  && exp_is_integer_arithmetic(from_)
  && exp_has_sum(to_)
  && distribute_mul_over_add_candidates(from_)
  |> List.exists(distributed =>
       exp_same_up_to_cleanup(distribution_cleanup, distributed, to_)
       || distribution_policy_allows_simplification(
            policy.Axioms.distribution_step_policy,
          )
       && polynomial_equivalent_exps(from_, to_)
     );
};

let exactly_one_mul_identity = (profile, from_, to_) =>
  if (!
        Axioms.visible_rule_enabled(
          profile.Axioms.step_policy,
          "arith.mul_identity",
        )) {
    false;
  } else {
    let from_ = from_ |> DHExp.strip_ascriptions |> strip_math_wrappers;
    switch (from_.term) {
    | BinOp(op, left, right) when is_times_op(op) =>
      switch (int_constant(left), int_constant(right)) {
      | (Some(value), _) when Bigint.equal(value, Bigint.one) =>
        exp_same(right, to_)
      | (_, Some(value)) when Bigint.equal(value, Bigint.one) =>
        exp_same(left, to_)
      | _ => false
      }
    | _ => false
    };
  };

let exactly_one_adjacent_const_fold = (left, right) => {
  let rec loop = (left, right) =>
    switch (left, right) {
    | ([l1, l2, ...left_rest], [r, ...right_rest]) =>
      switch (int_constant(l1), int_constant(l2)) {
      | (Some(left_value), Some(right_value)) =>
        let folded = int_exp(Bigint.(+)(left_value, right_value));
        if (Exp.fast_equal(folded, r)
            && List.length(left_rest) == List.length(right_rest)
            && List.for_all2(Exp.fast_equal, left_rest, right_rest)) {
          true;
        } else if (Exp.fast_equal(l1, r)) {
          loop([l2, ...left_rest], right_rest);
        } else {
          false;
        };
      | _ =>
        Exp.fast_equal(l1, r) ? loop([l2, ...left_rest], right_rest) : false
      }
    | _ => false
    };
  List.length(left) == List.length(right) + 1 && loop(left, right);
};

let check_single_arithmetic_rule_result_for_profile =
    (
      ~profile: Axioms.math_profile,
      ~settings as _: CoreSettings.t,
      ~env as _,
      from_,
      to_,
    ) => {
  switch (arithmetic_group_at_level(profile.level)) {
  | None => None
  | Some(group) =>
    let exact_rational_fold =
      switch (ArithmeticNormalization.fold_rational_constant(from_)) {
      | Some(folded) => Exp.fast_equal(folded, to_)
      | None => false
      };
    switch (flatten_addition(from_), flatten_addition(to_)) {
    | _ when exact_rational_fold =>
      let trace = trace_rules(group, ["arith.const_fold"]);
      Some({
        justification: "arithmetic one step",
        group: Some(group),
        from_normal_exp: from_ |> DHExp.strip_ascriptions,
        to_normal_exp: to_ |> DHExp.strip_ascriptions,
        from_trace: trace,
        to_trace: [],
        trace,
        prover_steps: [
          prover_step(
            ~origin=ManualRewrite,
            ~rule_id="arith.const_fold",
            ~before_full_exp=from_,
            ~after_full_exp=to_,
            ~before_exp=from_,
            ~after_exp=to_,
            ~detail="exact rational constant fold",
          ),
        ],
        exportable: true,
      });
    | (Some((_, from_terms)), Some((_, to_terms)))
        when exactly_one_adjacent_swap(from_terms, to_terms) =>
      let trace = trace_rules(group, ["arith.add_comm"]);
      Some({
        justification: "arithmetic one step",
        group: Some(group),
        from_normal_exp: from_ |> DHExp.strip_ascriptions,
        to_normal_exp: to_ |> DHExp.strip_ascriptions,
        from_trace: trace,
        to_trace: [],
        trace,
        prover_steps: [
          prover_step(
            ~origin=ManualRewrite,
            ~rule_id="arith.add_comm",
            ~before_full_exp=from_,
            ~after_full_exp=to_,
            ~before_exp=from_,
            ~after_exp=to_,
            ~detail="single adjacent addition swap",
          ),
        ],
        exportable: true,
      });
    | (Some((_, from_terms)), Some((_, to_terms)))
        when exactly_one_adjacent_const_fold(from_terms, to_terms) =>
      let trace = trace_rules(group, ["arith.const_fold"]);
      Some({
        justification: "arithmetic one step",
        group: Some(group),
        from_normal_exp: from_ |> DHExp.strip_ascriptions,
        to_normal_exp: to_ |> DHExp.strip_ascriptions,
        from_trace: trace,
        to_trace: [],
        trace,
        prover_steps: [
          prover_step(
            ~origin=ManualRewrite,
            ~rule_id="arith.const_fold",
            ~before_full_exp=from_,
            ~after_full_exp=to_,
            ~before_exp=from_,
            ~after_exp=to_,
            ~detail="single adjacent constant fold",
          ),
        ],
        exportable: true,
      });
    | _ when exactly_one_integer_distribution(profile, from_, to_) =>
      let trace = trace_rules(group, ["arith.mul_const"]);
      Some({
        justification: "arithmetic one step",
        group: Some(group),
        from_normal_exp: from_ |> DHExp.strip_ascriptions,
        to_normal_exp: to_ |> DHExp.strip_ascriptions,
        from_trace: trace,
        to_trace: [],
        trace,
        prover_steps: [
          prover_step(
            ~origin=ManualRewrite,
            ~rule_id="arith.mul_const",
            ~before_full_exp=from_,
            ~after_full_exp=to_,
            ~before_exp=from_,
            ~after_exp=to_,
            ~detail="single integer distribution",
          ),
        ],
        exportable: true,
      });
    | _ when exactly_one_mul_identity(profile, from_, to_) =>
      let trace = trace_rules(group, ["arith.mul_identity"]);
      Some({
        justification: "arithmetic one step",
        group: Some(group),
        from_normal_exp: from_ |> DHExp.strip_ascriptions,
        to_normal_exp: to_ |> DHExp.strip_ascriptions,
        from_trace: trace,
        to_trace: [],
        trace,
        prover_steps: [
          prover_step(
            ~origin=ManualRewrite,
            ~rule_id="arith.mul_identity",
            ~before_full_exp=from_,
            ~after_full_exp=to_,
            ~before_exp=from_,
            ~after_exp=to_,
            ~detail="remove multiplicative identity",
          ),
        ],
        exportable: true,
      });
    | _ => None
    };
  };
};

let check_single_arithmetic_rule_result_at_level =
    (~level, ~settings, ~env, from_, to_) =>
  check_single_arithmetic_rule_result_for_profile(
    ~profile=Axioms.math_profile(level),
    ~settings,
    ~env,
    from_,
    to_,
  );

let check_single_algebra_rule_result_for_profile =
    (
      ~profile: Axioms.math_profile,
      ~settings as _: CoreSettings.t,
      ~env as _,
      from_,
      to_,
    ) => {
  switch (algebra_group_at_level(profile.level)) {
  | None => None
  | Some(group) =>
    let from_ = from_ |> DHExp.strip_ascriptions |> strip_math_wrappers;
    let to_ = to_ |> DHExp.strip_ascriptions |> strip_math_wrappers;
    switch (check_single_algebra_identity(group, profile, from_, to_)) {
    | Some(result) => Some(result)
    | None =>
      switch (check_single_cancel_common_add(group, from_, to_)) {
      | Some(result) => Some(result)
      | None =>
        switch (
          check_single_division_distribution(group, profile, from_, to_)
        ) {
        | Some(result) => Some(result)
        | None =>
          switch (
            check_single_distribution_or_expansion(group, profile, from_, to_)
          ) {
          | Some(result) => Some(result)
          | None => check_single_factor_common(group, from_, to_)
          }
        }
      }
    };
  };
};

let check_single_algebra_rule_result_at_level =
    (~level, ~settings, ~env, from_, to_) =>
  check_single_algebra_rule_result_for_profile(
    ~profile=Axioms.math_profile(level),
    ~settings,
    ~env,
    from_,
    to_,
  );

let check_single_trig_rule_result_for_profile =
    (
      ~profile: Axioms.math_profile,
      ~settings as _: CoreSettings.t,
      ~env as _,
      from_,
      to_,
    ) => {
  switch (trigonometry_group_at_level(profile.level)) {
  | None => None
  | Some(group) =>
    let from_ = from_ |> DHExp.strip_ascriptions |> strip_math_wrappers;
    let to_ = to_ |> DHExp.strip_ascriptions |> strip_math_wrappers;
    TrigRewrite.applicable_at_root(from_)
    |> List.find_map((rewrite: TrigRewrite.rewrite) =>
         if (Axioms.visible_rule_enabled(profile.step_policy, rewrite.rule_id)
             && TrigRewrite.exp_same(rewrite.after_exp, to_)) {
           let trace = trace_rules(group, [rewrite.rule_id]);
           Some({
             justification: "trigonometry one step",
             group: Some(group),
             from_normal_exp: from_,
             to_normal_exp: to_,
             from_trace: trace,
             to_trace: [],
             trace,
             prover_steps: [
               prover_step(
                 ~origin=ManualRewrite,
                 ~rule_id=rewrite.rule_id,
                 ~before_full_exp=from_,
                 ~after_full_exp=to_,
                 ~before_exp=rewrite.before_exp,
                 ~after_exp=rewrite.after_exp,
                 ~detail="single trigonometry identity",
               ),
             ],
             exportable: false,
           });
         } else {
           None;
         }
       );
  };
};

let check_single_trig_rule_result_at_level =
    (~level, ~settings, ~env, from_, to_) =>
  check_single_trig_rule_result_for_profile(
    ~profile=Axioms.math_profile(level),
    ~settings,
    ~env,
    from_,
    to_,
  );

let check_single_calculus_rule_result_for_profile =
    (
      ~profile: Axioms.math_profile,
      ~settings as _: CoreSettings.t,
      ~env as _,
      from_,
      to_,
    ) => {
  switch (calculus_group_at_level(profile.level)) {
  | None => None
  | Some(group) =>
    let from_ = from_ |> DHExp.strip_ascriptions |> strip_math_wrappers;
    let to_ = to_ |> DHExp.strip_ascriptions |> strip_math_wrappers;
    let rule_enabled = rule_id =>
      Axioms.visible_rule_enabled(profile.step_policy, rule_id);
    let cleanup_trace = (rule_id, start) => {
      let capabilities =
        Axioms.cleanup_for_visible_rule(profile.step_policy, rule_id);
      let rec rounds = (fuel, current, rules, steps) =>
        if (fuel <= 0) {
          (current, rules, steps);
        } else {
          let (next, next_rules, next_steps) =
            capabilities
            |> List.fold_left(
                 ((current, rules, steps), capability) => {
                   let cleaned =
                     DifferentiationRewrite.cleanup(
                       ~cleanup_enabled=candidate => candidate == capability,
                       current,
                     );
                   if (TrigRewrite.exp_same(current, cleaned)) {
                     (current, rules, steps);
                   } else {
                     let metadata =
                       Axioms.cleanup_capability_metadata(capability);
                     let cleanup_rule: Axioms.rewrite_rule = {
                       id: metadata.id,
                       label: metadata.name,
                       prover_hints: [],
                     };
                     (
                       cleaned,
                       rules @ [cleanup_rule],
                       steps
                       @ [
                         prover_step(
                           ~origin=Normalization,
                           ~rule_id=metadata.id,
                           ~before_full_exp=current,
                           ~after_full_exp=cleaned,
                           ~before_exp=current,
                           ~after_exp=cleaned,
                           ~detail="profile cleanup after differentiation",
                         ),
                       ],
                     );
                   };
                 },
                 (current, rules, steps),
               );
          TrigRewrite.exp_same(current, next)
            ? (next, next_rules, next_steps)
            : rounds(fuel - 1, next, next_rules, next_steps);
        };
      rounds(8, start, [], []);
    };
    DifferentiationRewrite.applicable_at_root(~rule_enabled, from_)
    |> List.find_map((rewrite: TrigRewrite.rewrite) => {
         let (cleaned_after, cleanup_rules, cleanup_steps) =
           cleanup_trace(rewrite.rule_id, rewrite.after_exp);
         if (TrigRewrite.exp_same(rewrite.after_exp, to_)
             || TrigRewrite.exp_same(cleaned_after, to_)) {
           let visible_trace = trace_rules(group, [rewrite.rule_id]);
           let (trace, prover_steps) =
             if (TrigRewrite.exp_same(rewrite.after_exp, to_)) {
               (
                 visible_trace,
                 [
                   prover_step(
                     ~origin=ManualRewrite,
                     ~rule_id=rewrite.rule_id,
                     ~before_full_exp=from_,
                     ~after_full_exp=to_,
                     ~before_exp=rewrite.before_exp,
                     ~after_exp=rewrite.after_exp,
                     ~detail="single differentiation rule",
                   ),
                 ],
               );
             } else {
               (
                 visible_trace @ cleanup_rules,
                 [
                   prover_step(
                     ~origin=ManualRewrite,
                     ~rule_id=rewrite.rule_id,
                     ~before_full_exp=from_,
                     ~after_full_exp=rewrite.after_exp,
                     ~before_exp=rewrite.before_exp,
                     ~after_exp=rewrite.after_exp,
                     ~detail="single differentiation rule",
                   ),
                   ...cleanup_steps,
                 ],
               );
             };
           Some({
             justification: "calculus one step",
             group: Some(group),
             from_normal_exp: from_,
             to_normal_exp: to_,
             from_trace: trace,
             to_trace: [],
             trace,
             prover_steps,
             exportable: true,
           });
         } else {
           None;
         };
       });
  };
};

let check_single_calculus_rule_result_at_level =
    (~level, ~settings, ~env, from_, to_) =>
  check_single_calculus_rule_result_for_profile(
    ~profile=Axioms.math_profile(level),
    ~settings,
    ~env,
    from_,
    to_,
  );

let calculus_check_result_trace_for_profile =
    (~profile: Axioms.math_profile, from_, to_): option(trace_summary) =>
  if (!DifferentiationRewrite.contains_diff(from_)) {
    None;
  } else {
    switch (calculus_group_at_level(profile.level)) {
    | None => None
    | Some(group) =>
      /* Check Result may compose every visible calculus rule. Automatic
         cleanup is controlled independently below by default_cleanup, so
         hiding derivative.basics there must not disable the visible constant
         and variable derivative rules. */
      let rule_enabled = rule_id =>
        Axioms.visible_rule_enabled(profile.step_policy, rule_id);
      let rec differentiate = (fuel, current, rules, steps) =>
        if (fuel <= 0) {
          None;
        } else {
          switch (
            DifferentiationRewrite.rewrite_first(~rule_enabled, current)
          ) {
          | Some((next, rewrite)) =>
            let rule =
              Axioms.rewrite_rule_by_id(group, rewrite.rule_id)
              |> Option.value(
                   ~default={
                              id: rewrite.rule_id,
                              label: rewrite.label,
                              prover_hints: [],
                            }: Axioms.rewrite_rule,
                 );
            differentiate(
              fuel - 1,
              next,
              rules @ [rule],
              steps
              @ [
                prover_step(
                  ~origin=ManualRewrite,
                  ~rule_id=rewrite.rule_id,
                  ~before_full_exp=current,
                  ~after_full_exp=next,
                  ~before_exp=rewrite.before_exp,
                  ~after_exp=rewrite.after_exp,
                  ~detail="profile-directed differentiation",
                ),
              ],
            );
          | None => Some((current, rules, steps))
          };
        };
      let cleanup_enabled = capability =>
        List.mem(capability, profile.step_policy.default_cleanup);
      let rec cleanup_exact = (fuel, current, rules, steps) =>
        if (fuel <= 0) {
          None;
        } else {
          switch (
            DifferentiationRewrite.cleanup_once(~cleanup_enabled, current)
          ) {
          | None => Some((current, rules, steps))
          | Some((next, capability)) =>
            let metadata = Axioms.cleanup_capability_metadata(capability);
            let rule: Axioms.rewrite_rule = {
              id: metadata.id,
              label: metadata.name,
              prover_hints: [],
            };
            cleanup_exact(
              fuel - 1,
              next,
              rules @ [rule],
              steps
              @ [
                prover_step(
                  ~origin=Normalization,
                  ~rule_id=metadata.id,
                  ~before_full_exp=current,
                  ~after_full_exp=next,
                  ~before_exp=current,
                  ~after_exp=next,
                  ~detail="one profile cleanup rewrite",
                ),
              ],
            );
          };
        };
      let (initial, initial_rules, initial_steps) =
        switch (DifferentiationRewrite.diff_parts(from_)) {
        | Some((expression, variable)) =>
          switch (
            DifferentiationRewrite.strip(expression).term,
            DifferentiationRewrite.variable_name(variable),
          ) {
          | (Fun(pattern, body, _, _), Some(variable_name))
              when
                DifferentiationRewrite.function_parameter_name(pattern)
                == Some(variable_name)
                && List.mem(
                     Axioms.DerivativeBasics,
                     profile.step_policy.default_cleanup,
                   ) =>
            let initial = DifferentiationRewrite.diff_exp(body, variable);
            let metadata =
              Axioms.cleanup_capability_metadata(Axioms.DerivativeBasics);
            let rule: Axioms.rewrite_rule = {
              id: metadata.id,
              label: metadata.name,
              prover_hints: [],
            };
            (
              initial,
              [rule],
              [
                prover_step(
                  ~origin=Normalization,
                  ~rule_id=metadata.id,
                  ~before_full_exp=from_,
                  ~after_full_exp=initial,
                  ~before_exp=expression,
                  ~after_exp=body,
                  ~detail="differentiate the body of a matching function",
                ),
              ],
            );
          | _ => (from_, [], [])
          }
        | None => (from_, [], [])
        };
      switch (differentiate(128, initial, initial_rules, initial_steps)) {
      | None => None
      | Some((differentiated, rules, steps)) =>
        let cleaned = cleanup_exact(256, differentiated, rules, steps);
        switch (cleaned) {
        | None => None
        | Some((result, rules, steps)) =>
          let affine_finish = () => {
            let finish = (from_body, to_body) =>
              switch (
                rational_affine_trace_for_profile(
                  ~profile,
                  from_body,
                  to_body,
                )
              ) {
              | Some(_) as trace => trace
              | None =>
                check_with(
                  ~settings=CoreSettings.on,
                  ~env=Environment.empty,
                  from_body,
                  to_body,
                  affine_checker_at_level(profile.level),
                )
                |> Option.map(trace_summary_of_result)
              };
            switch (finish(result, to_)) {
            | Some(_) as finished => finished
            | None =>
              switch (
                DifferentiationRewrite.strip(result).term,
                DifferentiationRewrite.strip(to_).term,
              ) {
              | (
                  Fun(result_pattern, result_body, _, _),
                  Fun(target_pattern, target_body, _, _),
                )
                  when
                    DifferentiationRewrite.function_parameter_name(
                      result_pattern,
                    )
                    == DifferentiationRewrite.function_parameter_name(
                         target_pattern,
                       ) =>
                finish(result_body, target_body)
                |> Option.map((affine: trace_summary) =>
                     {
                       ...affine,
                       from_normal_exp: to_,
                       to_normal_exp: to_,
                       prover_steps:
                         affine.prover_steps
                         |> List.map((step: prover_step) =>
                              {
                                ...step,
                                before_full_exp: result,
                                after_full_exp: to_,
                              }
                            ),
                     }
                   )
              | _ => None
              }
            };
          };
          let finished =
            if (TrigRewrite.exp_same(result, to_)
                || Equality.ignoring_ascriptions.exp(result, to_)) {
              Some((result, rules, steps));
            } else if (Axioms.normalization_rule_id_enabled_for_profile(
                         profile,
                         Axioms.MultiStepCheck,
                         "arith.affine_normalize",
                       )) {
              affine_finish()
              |> Option.map((affine: trace_summary) => {
                   let affine_rules =
                     affine.rule_ids
                     |> List.map(rule_id => {
                          let label =
                            Axioms.catalog_rule_by_id(rule_id)
                            |> Option.map((rule: Axioms.math_rule) =>
                                 rule.metadata.name
                               )
                            |> Option.value(~default=rule_id);
                          {
                            Axioms.id: rule_id,
                            label,
                            prover_hints: [],
                          };
                        });
                   (to_, rules @ affine_rules, steps @ affine.prover_steps);
                 });
            } else {
              None;
            };
          switch (finished) {
          | None => None
          | Some((result, _rules, steps))
              when steps == [] || DifferentiationRewrite.contains_diff(result) =>
            None
          | Some((result, rules, steps)) =>
            let rule_ids =
              rules
              |> List.map((rule: Axioms.rewrite_rule) => rule.id)
              |> dedup;
            Some({
              justification: "profile-directed calculus",
              group_name: Some(group.name),
              from_normal_exp: result,
              to_normal_exp: result,
              from_rule_ids: rule_ids,
              to_rule_ids: [],
              rule_ids,
              prover_steps: steps,
              exportable: true,
            });
          };
        };
      };
    };
  };

let check_result_uses_rule = (rule: Axioms.math_rule, result) =>
  result.trace
  |> List.exists((trace_rule: Axioms.rewrite_rule) =>
       trace_rule.id == rule.id
     );

let check_single_catalog_rule =
    (
      ~profile,
      ~settings,
      ~env,
      from_,
      to_,
      planned_rule: Axioms.planned_visible_rule,
    ) => {
  let rule = planned_rule.rule;
  let rule_policy: Axioms.visible_rule_policy = {
    rule_id: rule.id,
    metadata: rule.metadata,
    allowed_cleanup: planned_rule.allowed_cleanup,
    session_rewrite: None,
  };
  let rule_profile: Axioms.math_profile = {
    ...profile,
    step_policy: {
      ...profile.Axioms.step_policy,
      visible_rules: [
        rule_policy,
        ...profile.step_policy.visible_rules
           |> List.filter((candidate: Axioms.visible_rule_policy) =>
                List.mem(candidate.rule_id, rule.required_rule_ids)
              ),
      ],
    },
  };
  let result =
    switch (rule.hazel_backend) {
    | Some(ArithmeticAddComm)
    | Some(ArithmeticConstFold)
    | Some(ArithmeticMulConst)
    | Some(ArithmeticMulIdentity) =>
      check_single_arithmetic_rule_result_for_profile(
        ~profile=rule_profile,
        ~settings,
        ~env,
        from_,
        to_,
      )
    | Some(ArithmeticScalarNormalize) =>
      let normalized =
        ArithmeticNormalization.simplify_scalar_products(from_);
      if (TrigRewrite.exp_same(normalized, to_)) {
        switch (arithmetic_group_at_level(profile.level)) {
        | None => None
        | Some(group) =>
          let trace = trace_rules(group, [rule.id]);
          Some({
            justification: "scalar/sign normalization",
            group: Some(group),
            from_normal_exp: from_,
            to_normal_exp: to_,
            from_trace: trace,
            to_trace: [],
            trace,
            prover_steps: [
              prover_step(
                ~origin=ManualRewrite,
                ~rule_id=rule.id,
                ~before_full_exp=from_,
                ~after_full_exp=to_,
                ~before_exp=from_,
                ~after_exp=to_,
                ~detail="focused arithmetic scalar/sign normalization",
              ),
            ],
            exportable: true,
          });
        };
      } else {
        None;
      };
    | Some(AlgebraDistributeMulAdd) =>
      switch (algebra_group_at_level(profile.level)) {
      | Some(group) =>
        check_single_distribution_or_expansion(
          group,
          rule_profile,
          from_,
          to_,
        )
      | None => None
      }
    | Some(AlgebraDistributeDivAdd) =>
      switch (algebra_group_at_level(profile.level)) {
      | Some(group) =>
        check_single_division_distribution(group, rule_profile, from_, to_)
      | None => None
      }
    | Some(AlgebraFactorCommon) =>
      switch (algebra_group_at_level(profile.level)) {
      | Some(group) => check_single_factor_common(group, from_, to_)
      | None => None
      }
    | Some(AlgebraCancelCommonAdd) =>
      switch (algebra_group_at_level(profile.level)) {
      | Some(group) => check_single_cancel_common_add(group, from_, to_)
      | None => None
      }
    | Some(AlgebraCollectLikeTerms) =>
      switch (algebra_group_at_level(profile.level)) {
      | Some(group) => check_single_collect_like_terms(group, from_, to_)
      | None => None
      }
    | Some(AlgebraIdentity) =>
      check_single_algebra_rule_result_for_profile(
        ~profile=rule_profile,
        ~settings,
        ~env,
        from_,
        to_,
      )
    | Some(TrigIdentity) =>
      check_single_trig_rule_result_for_profile(
        ~profile=rule_profile,
        ~settings,
        ~env,
        from_,
        to_,
      )
    | Some(CalculusDerivative) =>
      check_single_calculus_rule_result_for_profile(
        ~profile=rule_profile,
        ~settings,
        ~env,
        from_,
        to_,
      )
    | None => None
    };
  switch (result) {
  | Some(result) when check_result_uses_rule(rule, result) => Some(result)
  | _ => None
  };
};

let check_single_step_result_for_stage_plan =
    (
      ~profile,
      ~plan: Axioms.stage_plan,
      ~settings,
      ~env,
      from_: Exp.t,
      to_: Exp.t,
    ) =>
  plan.visible_rules
  |> List.find_map(rule =>
       check_single_catalog_rule(~profile, ~settings, ~env, from_, to_, rule)
     );

let check_single_session_rewrite_result =
    (~profile: Axioms.math_profile, from_: Exp.t, to_: Exp.t) =>
  Axioms.session_rewrites_for_profile(profile)
  |> List.find_map((definition: Axioms.session_rewrite) =>
       SessionRewrite.rewrites_at_root(definition, from_)
       |> List.find_opt((rewrite: TrigRewrite.rewrite) =>
            TrigRewrite.exp_same(rewrite.after_exp, to_)
          )
       |> Option.map((rewrite: TrigRewrite.rewrite) => {
            let rule: Axioms.rewrite_rule = {
              id: definition.id,
              label: definition.label,
              prover_hints: [],
            };
            {
              justification: "untrusted session rewrite",
              group: None,
              from_normal_exp: from_,
              to_normal_exp: to_,
              from_trace: [rule],
              to_trace: [],
              trace: [rule],
              prover_steps: [
                prover_step(
                  ~origin=ManualRewrite,
                  ~rule_id=definition.id,
                  ~before_full_exp=from_,
                  ~after_full_exp=to_,
                  ~before_exp=rewrite.before_exp,
                  ~after_exp=rewrite.after_exp,
                  ~detail=
                    "untrusted session-only rewrite; no Rocq certificate",
                ),
              ],
              exportable: false,
            };
          })
     );

/* Associativity is profile cleanup, so a learner may flatten or regroup an
 * entire ordered sum/product in one written step.  AxiomSearch still records
 * each primitive rotation, keeping the result replayable without treating
 * commutation or collection as implicit. */
let association_cleanup_result_for_profile =
    (~profile: Axioms.math_profile, from_: Exp.t, to_: Exp.t) => {
  let cleanup = profile.step_policy.default_cleanup;
  let association_capabilities =
    [Axioms.AddAssoc, Axioms.MulAssoc]
    |> List.filter(capability => List.mem(capability, cleanup));
  let allowed_rule_ids =
    association_capabilities |> List.map(Axioms.primitive_rule_id_for_cleanup);
  let rec operator_count = exp =>
    switch (strip_math_wrappers(exp).term) {
    | BinOp(op, left, right) =>
      (is_plus_op(op) || is_times_op(op) ? 1 : 0)
      + operator_count(left)
      + operator_count(right)
    | UnOp(_, inner)
    | Parens(inner) => operator_count(inner)
    | Ap(_, fn, arg) => operator_count(fn) + operator_count(arg)
    | _ => 0
    };
  if (allowed_rule_ids == []
      || same_math_exp(from_, to_)
      || !exp_same_up_to_cleanup(association_capabilities, from_, to_)) {
    None;
  } else {
    let max_depth = max(1, operator_count(from_) + operator_count(to_));
    AxiomSearch.search(
      ~level=profile.level,
      ~max_depth,
      ~max_states=max(250, max_depth * 100),
      ~allowed_rule_ids,
      ~log=false,
      from_,
      to_,
    )
    |> Option.map((result: AxiomSearch.result) => {
         let trace =
           result.applications
           |> List.map((app: AxiomSearch.application) => app.rule)
           |> List.fold_left(
                (rules, rule: Axioms.rewrite_rule) =>
                  rules
                  |> List.exists((candidate: Axioms.rewrite_rule) =>
                       candidate.id == rule.id
                     )
                    ? rules : rules @ [rule],
                [],
              );
         {
           justification: "association cleanup",
           group: arithmetic_group_at_level(profile.level),
           from_normal_exp: to_,
           to_normal_exp: to_,
           from_trace: trace,
           to_trace: [],
           trace,
           prover_steps: result.steps,
           exportable: result.steps != [],
         };
       });
  };
};

let check_single_step_result_for_profile =
    (~profile: Axioms.math_profile, ~settings, ~env, from_: Exp.t, to_: Exp.t) => {
  let plan = Axioms.stage_plan_for_profile(profile, Manual);
  switch (
    check_single_step_result_for_stage_plan(
      ~profile,
      ~plan,
      ~settings,
      ~env,
      from_,
      to_,
    )
  ) {
  | Some(result) => Some(result)
  | None =>
    switch (association_cleanup_result_for_profile(~profile, from_, to_)) {
    | Some(result) => Some(result)
    | None =>
      switch (check_single_session_rewrite_result(~profile, from_, to_)) {
      | Some(result) => Some(result)
      | None => check_single_eval_step_result(~settings, ~env, from_, to_)
      }
    }
  };
};

let check_single_step_result_at_level =
    (~level, ~settings, ~env, from_: Exp.t, to_: Exp.t) =>
  check_single_step_result_for_profile(
    ~profile=Axioms.math_profile(level),
    ~settings,
    ~env,
    from_,
    to_,
  );

let check_single_step_trace_at_level =
    (~level, ~settings, ~env, from_: Exp.t, to_: Exp.t)
    : option(trace_summary) =>
  check_single_step_result_at_level(~level, ~settings, ~env, from_, to_)
  |> Option.map(trace_summary_of_result);

let written_step_checkers_at_level = level => [
  affine_checker_at_level(level),
  algebra_distribution_checker_at_level(level),
  evaluation_checker,
];

let written_step_checkers = written_step_checkers_at_level(Axioms.Arithmetic);

// underscores indicate unused arguments
let check_rewrite_at_level =
    (~level, ~settings, ~env, from_: Exp.t, to_: Exp.t): bool => {
  switch (
    check_with(~settings, ~env, from_, to_, affine_checker_at_level(level))
  ) {
  | Some(_) => true
  | None => false
  };
};

let check_rewrite = (~settings, ~env, from_: Exp.t, to_: Exp.t): bool =>
  check_rewrite_at_level(
    ~level=Axioms.Arithmetic,
    ~settings,
    ~env,
    from_,
    to_,
  );

let check_rewrite_result_at_level =
    (~level, ~settings, ~env, from_: Exp.t, to_: Exp.t) =>
  check_with(~settings, ~env, from_, to_, affine_checker_at_level(level));

let check_rewrite_result = (~settings, ~env, from_: Exp.t, to_: Exp.t) =>
  check_rewrite_result_at_level(
    ~level=Axioms.Arithmetic,
    ~settings,
    ~env,
    from_,
    to_,
  );

let check_written_step_result_at_level =
    (~level, ~settings, ~env, from_: Exp.t, to_: Exp.t): option(check_result) => {
  switch (
    check_single_calculus_rule_result_at_level(
      ~level,
      ~settings,
      ~env,
      from_,
      to_,
    )
  ) {
  | Some(result) => Some(result)
  | None =>
    switch (
      check_single_trig_rule_result_at_level(
        ~level,
        ~settings,
        ~env,
        from_,
        to_,
      )
    ) {
    | Some(result) => Some(result)
    | None =>
      written_step_checkers_at_level(level)
      |> List.find_map(checker =>
           check_with(~settings, ~env, from_, to_, checker)
         )
    }
  };
};

let check_written_step_result =
    (~settings, ~env, from_: Exp.t, to_: Exp.t): option(check_result) =>
  check_written_step_result_at_level(
    ~level=Axioms.Arithmetic,
    ~settings,
    ~env,
    from_,
    to_,
  );

let check_written_step_at_level =
    (~level, ~settings, ~env, from_: Exp.t, to_: Exp.t): option(string) => {
  check_written_step_result_at_level(~level, ~settings, ~env, from_, to_)
  |> Option.map((result: check_result) => result.justification);
};

let check_written_step_trace_at_level =
    (~level, ~settings, ~env, from_: Exp.t, to_: Exp.t)
    : option(trace_summary) => {
  check_written_step_result_at_level(~level, ~settings, ~env, from_, to_)
  |> Option.map(trace_summary_of_result);
};

let trace_rule_allowed_by_profile = (profile: Axioms.math_profile, rule_id) => {
  let capability_id =
    switch (Axioms.cleanup_capability_for_id(rule_id)) {
    | Some(capability) => Axioms.cleanup_capability_label(capability)
    | None => rule_id
    };
  Axioms.compiled_capability_enabled(
    Axioms.stage_plan_for_profile(profile, Axioms.MultiStepCheck),
    capability_id,
  );
};

let direct_cleanup_trace_for_profile =
    (~profile: Axioms.math_profile, from_: Exp.t, to_: Exp.t)
    : option(trace_summary) => {
  let enabled = capability =>
    List.mem(capability, profile.step_policy.default_cleanup);
  let summary = (rule_ids, steps) =>
    Some({
      justification: "profile cleanup",
      group_name: None,
      from_normal_exp: to_,
      to_normal_exp: to_,
      from_rule_ids: rule_ids,
      to_rule_ids: [],
      rule_ids,
      prover_steps: steps,
      exportable: true,
    });
  let rec rewrite_cleanup = (fuel, current, rule_ids, steps) =>
    if (fuel <= 0) {
      None;
    } else if (steps != [] && TrigRewrite.exp_same(current, to_)) {
      summary(rule_ids |> dedup, steps);
    } else {
      switch (
        DifferentiationRewrite.cleanup_once(~cleanup_enabled=enabled, current)
      ) {
      | Some((next, capability)) =>
        let rule_id = Axioms.cleanup_capability_label(capability);
        rewrite_cleanup(
          fuel - 1,
          next,
          rule_ids @ [rule_id],
          steps
          @ [
            prover_step(
              ~origin=Normalization,
              ~rule_id,
              ~before_full_exp=current,
              ~after_full_exp=next,
              ~before_exp=current,
              ~after_exp=next,
              ~detail="profile-enabled direct cleanup",
            ),
          ],
        );
      | None => None
      };
    };
  switch (rewrite_cleanup(64, from_, [], [])) {
  | Some(summary) => Some(summary)
  | None =>
    switch (association_cleanup_result_for_profile(~profile, from_, to_)) {
    | Some(result) => Some(trace_summary_of_result(result))
    | None
        when
          enabled(Axioms.PowerNotation)
          && exp_same_up_to_cleanup([Axioms.PowerNotation], from_, to_) =>
      let rule_id = Axioms.cleanup_capability_label(Axioms.PowerNotation);
      summary(
        [rule_id],
        [
          prover_step(
            ~origin=Normalization,
            ~rule_id,
            ~before_full_exp=from_,
            ~after_full_exp=to_,
            ~before_exp=from_,
            ~after_exp=to_,
            ~detail="profile-enabled power notation cleanup",
          ),
        ],
      );
    | None => None
    }
  };
};

let check_written_step_trace_for_profile =
    (
      ~stage=Axioms.MultiStepCheck,
      ~profile: Axioms.math_profile,
      ~settings,
      ~env,
      from_: Exp.t,
      to_: Exp.t,
    )
    : option(trace_summary) => {
  let requires_disabled_power_notation =
    !List.mem(Axioms.PowerNotation, profile.step_policy.default_cleanup)
    && !TrigRewrite.exp_same(from_, to_)
    && exp_same_up_to_cleanup([Axioms.PowerNotation], from_, to_);
  if (requires_disabled_power_notation) {
    None;
  } else {
    let candidate =
      switch (direct_cleanup_trace_for_profile(~profile, from_, to_)) {
      | Some(summary) => Some(summary)
      | None =>
        switch (
          check_single_step_result_for_stage_plan(
            ~profile,
            ~plan=Axioms.stage_plan_for_profile(profile, stage),
            ~settings,
            ~env,
            from_,
            to_,
          )
        ) {
        | Some(result) when result.justification == "algebra one step" =>
          Some(trace_summary_of_result(result))
        | Some(_)
        | None =>
          switch (
            calculus_check_result_trace_for_profile(~profile, from_, to_)
          ) {
          | Some(summary) => Some(summary)
          | None =>
            switch (
              rational_polynomial_expansion_trace_for_profile(
                ~profile,
                ~stage,
                from_,
                to_,
              )
            ) {
            | Some(summary) => Some(summary)
            | None =>
              switch (rational_affine_trace_for_profile(~profile, from_, to_)) {
              | Some(summary) => Some(summary)
              | None =>
                check_written_step_trace_at_level(
                  ~level=profile.level,
                  ~settings,
                  ~env,
                  from_,
                  to_,
                )
              }
            }
          }
        }
      };
    let candidate =
      switch (candidate) {
      | Some(summary)
          when
            summary.justification == "arithmetic"
            && Axioms.normalization_rule_id_enabled_for_profile(
                 profile,
                 Axioms.MultiStepCheck,
                 "arith.affine_normalize",
               ) =>
        let rule_id = "arith.affine_normalize";
        Some({
          ...summary,
          justification: "affine normalization",
          from_normal_exp: to_,
          to_normal_exp: to_,
          from_rule_ids: [rule_id],
          to_rule_ids: [],
          rule_ids: [rule_id],
          prover_steps: [
            prover_step(
              ~origin=Normalization,
              ~rule_id,
              ~before_full_exp=from_,
              ~after_full_exp=to_,
              ~before_exp=from_,
              ~after_exp=to_,
              ~detail="profile-enabled affine normalization",
            ),
          ],
          exportable: true,
        });
      | Some(_)
      | None => candidate
      };
    switch (candidate) {
    | Some(summary)
        when
          summary.rule_ids
          |> List.for_all(trace_rule_allowed_by_profile(profile)) =>
      Some(summary)
    | Some(_)
    | None => None
    };
  };
};

let check_written_step_trace =
    (~settings, ~env, from_: Exp.t, to_: Exp.t): option(trace_summary) => {
  check_written_step_trace_for_profile(
    ~profile=Axioms.math_profile(Axioms.Arithmetic),
    ~settings,
    ~env,
    from_,
    to_,
  );
};

let check_written_step =
    (~settings, ~env, from_: Exp.t, to_: Exp.t): option(string) => {
  check_written_step_at_level(
    ~level=Axioms.Arithmetic,
    ~settings,
    ~env,
    from_,
    to_,
  );
};
