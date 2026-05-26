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

type checker = {
  justification: string,
  normalize:
    (~settings: CoreSettings.t, ~env: Environment.t(Exp.t), Exp.t) =>
    option(normal_form),
  equivalent: (normal_form, normal_form) => bool,
};

let is_zero = Bigint.equal(Bigint.zero);

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

let rec affine_of_exp = (exp: Exp.t): option(affine) =>
  switch (exp.term) {
  | Atom(Int(value))
  | Atom(Nat(value)) => Some(affine_const(value))
  | Atom(SInt(value)) => Some(affine_const(Bigint.of_int(value)))
  | Var(name) => Some(affine_var(name))
  | Parens(exp)
  | Asc(exp, _) => affine_of_exp(exp)
  | UnOp(Int(Minus) | SInt(Minus), exp) =>
    affine_of_exp(exp) |> Option.map(affine_negate)
  | BinOp(Int(Plus) | Nat(Plus) | SInt(Plus), left, right) =>
    switch (affine_of_exp(left), affine_of_exp(right)) {
    | (Some(left), Some(right)) => Some(affine_add(left, right))
    | _ => None
    }
  | BinOp(Int(Minus) | SInt(Minus), left, right) =>
    switch (affine_of_exp(left), affine_of_exp(right)) {
    | (Some(left), Some(right)) => Some(affine_sub(left, right))
    | _ => None
    }
  | BinOp(Int(Times) | Nat(Times) | SInt(Times), left, right) =>
    switch (affine_of_exp(left), affine_of_exp(right)) {
    | (Some(left), Some(right)) =>
      switch (affine_constant(left), affine_constant(right)) {
      | (Some(coeff), _) => Some(affine_scale(coeff, right))
      | (_, Some(coeff)) => Some(affine_scale(coeff, left))
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
  |> Option.map(a => Affine(canonicalize(a)));
};

let normalize_by_evaluation =
    (~settings as _: CoreSettings.t, ~env, exp: Exp.t): option(normal_form) => {
  switch (Evaluator.evaluate_and_limit(~env, ~step_limit=1000, exp)) {
  | Completed((value, _)) =>
    Some(Evaluated(value |> DHExp.strip_ascriptions))
  | StepLimitExceeded => None
  | exception _ => None
  };
};

let affine_checker = {
  justification: "arithmetic",
  normalize: normalize_affine,
  equivalent: (left, right) =>
    switch (left, right) {
    | (Affine(left), Affine(right)) => left == right
    | _ => false
    },
};

let evaluation_checker = {
  justification: "same evaluated result",
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
      when checker.equivalent(from_normal, to_normal) =>
    Some(checker.justification)
  | _ => None
  };
};

let written_step_checkers = [affine_checker, evaluation_checker];

// underscores indicate unused arguments
let check_rewrite = (~settings, ~env, from_: Exp.t, to_: Exp.t): bool => {
  switch (check_with(~settings, ~env, from_, to_, affine_checker)) {
  | Some(_) => true
  | None => false
  };
};

let check_written_step =
    (~settings, ~env, from_: Exp.t, to_: Exp.t): option(string) => {
  written_step_checkers
  |> List.find_map(checker =>
       check_with(~settings, ~env, from_, to_, checker)
     );
};
