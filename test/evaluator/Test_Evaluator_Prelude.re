open Alcotest;
open Haz3lcore;

module UG = Grammar.UnitGrammar;

let dhexp_typ = testable(Fmt.using(Exp.show, Fmt.string), DHExp.fast_equal);

let evaluation_test = (msg, expected, unevaluated) =>
  check(
    dhexp_typ,
    msg,
    expected,
    unevaluated |> Evaluator.evaluate(~env=Builtins.env_init) |> fst,
  );

let evaluate_probes = unevaluated =>
  unevaluated
  |> Evaluator.evaluate(~env=Builtins.env_init)
  |> snd
  |> EvaluatorState.get_probes;

let parse_exp = (s: string) => {
  switch (Parse.parse_exp(s)) {
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
let parse_and_evaluate = (s: string) =>
  fst(Evaluator.evaluate(~env=Builtins.env_init, elaborate(parse_exp(s))));

let parse_and_evaluate_test =
    (~msg: option(string)=?, expected: string, actual: string) =>
  evaluation_test(
    Option.value(~default=expected ++ " == " ++ actual, msg),
    parse_exp(expected),
    elaborate(parse_exp(actual)),
  );

let step_limited = (t: Alcotest.testable('a)) =>
  testable(
    Fmt.using(Evaluator.show_step_constrained(pp(t)), Fmt.string),
    Evaluator.equal_step_constrained(equal(t)),
  );

let full_small_step_reduction =
    (~state=EvaluatorState.init, ~step_limit=1000, exp: TermBase.exp_t)
    : Evaluator.step_constrained(Exp.t) => {
  let rec go =
          (~state=EvaluatorState.init, ~steps_counter=0, exp: TermBase.exp_t)
          : option((Exp.t, EvaluatorState.t)) => {
    switch (EvaluatorStep.get_status(~settings=CoreSettings.on, exp, state)) {
    | _ when steps_counter > step_limit => None
    | AutoStep(step)
    | AvailableSteps([step, ..._]) =>
      switch (EvaluatorStep.take_step(step)) {
      | Some((new_exp, new_state)) =>
        go(~state=new_state, ~steps_counter=steps_counter + 1, new_exp)
      | None => Some((exp, state))
      }
    | AvailableSteps([]) => Some((exp, state))
    };
  };

  switch (go(~state, ~steps_counter=0, exp)) {
  | None => StepLimitExceeded
  | Some((new_exp, _)) => Completed(new_exp)
  };
};
