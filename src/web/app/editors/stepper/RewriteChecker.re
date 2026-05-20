open Util;
open Language;
open Js_of_ocaml;

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

type normal_form =
  | Evaluated(Exp.t)
  | Algebraic(string);

type checker = {
  justification: string,
  normalize:
    (~settings: CoreSettings.t, ~env: Environment.t(Exp.t), Exp.t) =>
    option(normal_form),
  equivalent: (normal_form, normal_form) => bool,
};

let algebrite_bin_op = (op: Operators.op_bin): option(string) =>
  switch (op) {
  | Int(Plus)
  | Nat(Plus)
  | SInt(Plus)
  | Float(Plus) => Some("+")
  | Int(Minus)
  | Nat(Minus)
  | SInt(Minus)
  | Float(Minus) => Some("-")
  | Int(Times)
  | Nat(Times)
  | SInt(Times)
  | Float(Times) => Some("*")
  | Int(Power)
  | Nat(Power)
  | SInt(Power)
  | Float(Power) => Some("^")
  | Int(Divide)
  | Nat(Divide)
  | SInt(Divide)
  | Float(Divide) => Some("/")
  | Int(LessThan | LessThanOrEqual | GreaterThan | GreaterThanOrEqual)
  | Nat(LessThan | LessThanOrEqual | GreaterThan | GreaterThanOrEqual)
  | SInt(LessThan | LessThanOrEqual | GreaterThan | GreaterThanOrEqual)
  | Float(
      LessThan | LessThanOrEqual | GreaterThan | GreaterThanOrEqual | Equals |
      NotEquals,
    )
  | Bool(_)
  | String(_)
  | Poly(_) => None
  };

let rec print_exp_for_algebrite = (exp: Exp.t): option(string) =>
  switch (exp.term) {
  | Atom(Int(value))
  | Atom(Nat(value)) => Some(Bigint.to_string(value))
  | Atom(SInt(value)) => Some(string_of_int(value))
  | Atom(Float(value)) => Some(string_of_float(value))
  | Var(value) => Some(value)
  | BinOp(op, exp_left, exp_right) =>
    switch (
      algebrite_bin_op(op),
      print_exp_for_algebrite(exp_left),
      print_exp_for_algebrite(exp_right),
    ) {
    | (Some(op), Some(left), Some(right)) =>
      Some("(" ++ left ++ " " ++ op ++ " " ++ right ++ ")")
    | _ => None
    }
  | UnOp(Int(Minus) | Nat(Minus) | SInt(Minus) | Float(Minus), exp) =>
    switch (print_exp_for_algebrite(exp)) {
    | Some(exp) => Some("(-" ++ exp ++ ")")
    | None => None
    }
  | Parens(exp) =>
    switch (print_exp_for_algebrite(exp)) {
    | Some(exp) => Some("(" ++ exp ++ ")")
    | None => None
    }
  // TODO: think harder about weird corner cases where we'd want to ensure the types in Cast are valid
  | Asc(exp, _) => print_exp_for_algebrite(exp)
  | Atom(Bool(_) | String(_))
  | UnOp(Bool(_), _)
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

let checkEquality = (expr1, expr2): bool => {
  let algebrite = Js.Unsafe.global##.Algebrite;
  let diffExpr = Printf.sprintf("simplify((%s)-(%s))", expr1, expr2);
  let algebrite_result = algebrite##run(Js.string(diffExpr));
  switch (Js.to_string(algebrite_result)) {
  | "0" => true
  | _ => false
  };
};

let normalize_arithmetic = (~settings, ~env, exp: Exp.t): option(normal_form) => {
  exp
  |> DHExp.strip_ascriptions
  |> take_auto_steps(~settings, ~env)
  |> print_exp_for_algebrite
  |> Option.map(s => Algebraic(s));
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

let arithmetic_checker = {
  justification: "arithmetic",
  normalize: normalize_arithmetic,
  equivalent: (left, right) =>
    switch (left, right) {
    | (Algebraic(left), Algebraic(right)) => checkEquality(left, right)
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

let written_step_checkers = [arithmetic_checker, evaluation_checker];

// underscores indicate unused arguments
let check_rewrite = (~settings, ~env, from_: Exp.t, to_: Exp.t): bool => {
  switch (check_with(~settings, ~env, from_, to_, arithmetic_checker)) {
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
