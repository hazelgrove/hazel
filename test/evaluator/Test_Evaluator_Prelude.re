open Alcotest;
open Language;

module UG = Grammar.UnitGrammar;

let testable_exp = (~ignore_constructor_types=?, ()) =>
  testable(
    Fmt.using(Exp.show, Fmt.string),
    DHExp.fast_equal(~ignore_constructor_types?),
  );
let evaluate = unevaluated =>
  unevaluated |> Evaluator.evaluate(~env=Builtins.env_init) |> fst;
let dhexp_typ = testable_exp();

let evaluation_test =
    (~ignore_constructor_types=?, msg, expected, unevaluated) =>
  check(
    testable_exp(~ignore_constructor_types?, ()),
    msg,
    expected,
    evaluate(unevaluated),
  );

let evaluate_probes = unevaluated =>
  unevaluated
  |> Evaluator.evaluate(~env=Builtins.env_init)
  |> snd
  |> EvaluatorState.get_probes;

let parse_exp = (s: string) => {
  switch (
    Haz3lcore.Parser.to_term(
      ~projector_init=Haz3lcore.Parser.default_projector_init,
      s,
    )
  ) {
  | Some(e) => e
  | None => Alcotest.fail("Failed to parse expression: " ++ s)
  };
};
let elaborate = u =>
  Elaborator.elaborate(
    Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), u),
    u,
  )
  |> fst;

(exp, probes) => (
  {
    term: exp,
    annotation: probes,
  }:
    Grammar.pat_t(list(Grammar.exp_t(unit)))
);
let parse_and_evaluate = (s: string) => evaluate(elaborate(parse_exp(s)));

let parse_and_evaluate_test =
    (
      ~msg: option(string)=?,
      ~ignore_constructor_types=?,
      expected: string,
      actual: string,
    ) =>
  evaluation_test(
    ~ignore_constructor_types?,
    Option.value(~default=expected ++ " == " ++ actual, msg),
    parse_exp(expected),
    elaborate(parse_exp(actual)),
  );

let step_limited = (t: Alcotest.testable('a)) =>
  testable(
    Fmt.using(Evaluator.show_step_constrained(pp(t)), Fmt.string),
    Evaluator.equal_step_constrained(equal(t)),
  );
let single_step = (exp: Exp.t) => {
  let step =
    EvaluatorStep.get_status(
      ~settings=CoreSettings.on,
      exp,
      EvaluatorState.init,
    );
  switch (step) {
  | AutoStep(step) => EvaluatorStep.take_step(step)
  | AvailableSteps([step, ..._]) => EvaluatorStep.take_step(step)
  | AvailableSteps([]) => None
  };
};

let full_small_step_reduction =
    (~state=EvaluatorState.init, ~step_limit=1000, exp: TermBase.exp_t)
    : Evaluator.step_constrained(Exp.t) => {
  let rec go =
          (~state=EvaluatorState.init, ~steps_counter=0, exp: TermBase.exp_t)
          : option((Exp.t, EvaluatorState.t)) =>
    if (steps_counter > step_limit) {
      None;
    } else {
      switch (single_step(exp)) {
      | Some((new_exp, new_state)) =>
        go(~state=new_state, ~steps_counter=steps_counter + 1, new_exp)
      | None => Some((exp, state))
      };
    };

  switch (go(~state, ~steps_counter=0, exp)) {
  | None => StepLimitExceeded
  | Some((new_exp, _)) => Completed(new_exp)
  };
};

let full_preservation_test = (uexp: TermBase.exp_t): unit => {
  let statics =
    Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), uexp);
  let (elaborated, ty) = Elaborator.elaborate(statics, uexp);

  let evaluated =
    Evaluator.evaluate(~env=Builtins.env_init, elaborated) |> fst;
  let new_statics =
    Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), evaluated);

  let new_ty =
    switch (
      Statics.Map.lookup(evaluated.annotation.ids |> List.hd, new_statics)
    ) {
    | Some(InfoExp({ty, _})) => ty
    | _ =>
      Alcotest.fail(
        "Preservation check failed: No type information found for evaluated expression",
      )
    };

  if (Typ.is_consistent(Ctx.empty, new_ty, ty)) {
    ();
  } else {
    Alcotest.fail(
      "Preservation check failed: "
      ++ Typ.show(ty)
      ++ " !~ "
      ++ Typ.show(new_ty),
    );
  };
};
