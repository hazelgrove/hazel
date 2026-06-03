open Util;
open Language;

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

type normal_form =
  | Evaluated(Exp.t)
  | Affine(affine);

type normalized = {
  normal_form,
  rule_ids: list(string),
};

type check_result = {
  justification: string,
  group: option(Axioms.rewrite_group),
  trace: list(Axioms.rewrite_rule),
  exportable: bool,
};

type checker = {
  justification: string,
  group: option(Axioms.rewrite_group),
  normalize:
    (~settings: CoreSettings.t, ~env: Environment.t(Exp.t), Exp.t) =>
    option(normalized),
  equivalent: (normal_form, normal_form) => bool,
};

let is_zero = Bigint.equal(Bigint.zero);

let dedup = values =>
  values
  |> List.fold_left(
       (acc, value) => List.mem(value, acc) ? acc : [value, ...acc],
       [],
     )
  |> List.rev;

let trace_rules = (group, rule_ids) =>
  rule_ids
  |> dedup
  |> List.filter_map(rule_id => Axioms.rewrite_rule_by_id(group, rule_id));

let group_named_at_level = (level, name) =>
  Axioms.allowed_groups(level)
  |> List.find_opt((group: Axioms.rewrite_group) => group.name == name);

let arithmetic_group_at_level = level =>
  group_named_at_level(level, "arithmetic");

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
  | Var(name) => Some(affine_normalization(affine_var(name), []))
  | Parens(exp)
  | Asc(exp, _) => affine_of_exp(exp)
  | UnOp(Int(Minus) | SInt(Minus), exp) =>
    affine_of_exp(exp)
    |> Option.map(normalized =>
         affine_normalization(
           affine_negate(normalized.affine),
           ["arith.add_neg", ...normalized.rule_ids],
         )
       )
  | BinOp(Int(Plus) | Nat(Plus) | SInt(Plus), left, right) =>
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
  | BinOp(Int(Minus) | SInt(Minus), left, right) =>
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
  | BinOp(Int(Times) | Nat(Times) | SInt(Times), left, right) =>
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
  | Atom(Float(_) | Bool(_) | String(_))
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

let int_exp = value => Exp.fresh(Atom(Int(value)));

let var_exp = name => Exp.fresh(Var(name));

let plus_exp = (left, right) =>
  Exp.fresh(BinOp(Operators.Int(Operators.Plus), left, right));

let times_exp = (left, right) =>
  Exp.fresh(BinOp(Operators.Int(Operators.Times), left, right));

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

let normalize_affine_with_trace =
    (~settings, ~env, exp: Exp.t): option(normalized) => {
  exp
  |> DHExp.strip_ascriptions
  |> take_auto_steps(~settings, ~env)
  |> affine_of_exp
  |> Option.map(normalized =>
       {
         normal_form: Affine(canonicalize(normalized.affine)),
         rule_ids: normalized.rule_ids,
       }
     );
};

let normalize_by_evaluation =
    (~settings as _: CoreSettings.t, ~env, exp: Exp.t): option(normalized) => {
  switch (Evaluator.evaluate_and_limit(~env, ~step_limit=1000, exp)) {
  | Completed((value, _)) =>
    Some({
      normal_form: Evaluated(value |> DHExp.strip_ascriptions),
      rule_ids: [],
    })
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
    let trace =
      switch (checker.group) {
      | Some(group) =>
        trace_rules(group, from_normal.rule_ids @ to_normal.rule_ids)
      | None => []
      };
    Some({
      justification: checker.justification,
      group: checker.group,
      trace,
      exportable: trace != [],
    });
  | _ => None
  };
};

let written_step_checkers_at_level = level => [
  affine_checker_at_level(level),
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
  written_step_checkers_at_level(level)
  |> List.find_map(checker =>
       check_with(~settings, ~env, from_, to_, checker)
     );
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
