open Language;
open Util;

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

/* Evaluate with a probe_map to collect probe samples.
 * The probe_map tells the evaluator which expressions to record. */
let evaluate_with_probe_map =
    (~sample_map: Id.Map.t(Sample.capture_spec), exp: Exp.t)
    : (Exp.t, Sample.Map.t) => {
  let elaborated = elaborate(exp);
  let (result, state) =
    Evaluator.evaluate(
      ~targets=sample_map,
      ~env=Builtins.env_init,
      elaborated,
    );
  (result, state.probes);
};

/* Evaluate and return both the result and test results */
let evaluate_with_tests = (exp: Exp.t): (Exp.t, TestResults.t) => {
  let (result, state) =
    Evaluator.evaluate(~env=Builtins.env_init, elaborate(exp));
  let test_results = TestResults.mk_results(EvaluatorState.get_tests(state));
  (result, test_results);
};
