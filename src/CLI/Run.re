open Language;

let elaborate = (exp: Exp.t): Exp.t =>
  fst(
    Elaborator.elaborate(
      Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), exp),
      exp,
    ),
  );

let evaluate = (exp: Exp.t): Exp.t =>
  fst(Evaluator.evaluate(~env=Builtins.env_init, elaborate(exp)));

/* Evaluate and return both the result and the probe sample map */
let evaluate_with_probes = (exp: Exp.t): (Exp.t, Sample.Map.t) => {
  let (result, state) =
    Evaluator.evaluate(~env=Builtins.env_init, elaborate(exp));
  (result, state.probes);
};

/* Evaluate and return both the result and test results */
let evaluate_with_tests = (exp: Exp.t): (Exp.t, TestResults.t) => {
  let (result, state) =
    Evaluator.evaluate(~env=Builtins.env_init, elaborate(exp));
  let test_results = TestResults.mk_results(EvaluatorState.get_tests(state));
  (result, test_results);
};
