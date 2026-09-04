open Language;
open Util_web;

let statics_and_elab = (exp: Exp.t): (Statics.Map.t, Exp.t) =>
  Statics.mk(
    CoreSettings.on,
    Builtins.ctx_init(Some(Operators.default_mode)),
    exp,
  );

let elaborate = (exp: Exp.t): Exp.t => snd(statics_and_elab(exp));

let evaluate = (exp: Exp.t): Exp.t => {
  let (result, _) =
    Evaluator.evaluate(~env=Builtins.env_init, elaborate(exp));
  result;
};

/* Eval-only entry points for `hazel bench-eval`: parse/statics excluded. */
let elab_and_eval_info = (exp: Exp.t): (Exp.t, EvalInfo.t) => {
  let (info_map, elab) = statics_and_elab(exp);
  (
    elab,
    EvalInfo.of_info_map(
      ~probe_all=CoreSettings.on.probe_all,
      ~targets=Id.Map.empty,
      info_map,
    ),
  );
};

let evaluate_elab = (elab: Exp.t): Exp.t =>
  fst(Evaluator.evaluate(~env=Builtins.env_init, elab));

let evaluate_elab_incr = (~eval_info: EvalInfo.t, elab: Exp.t): Exp.t =>
  fst(Evaluator.evaluate(~eval_info, ~env=Builtins.env_init, elab));

/* Evaluate with a probe_map to collect probe samples.
 * The probe_map tells the evaluator which expressions to record. */
let evaluate_with_probe_map =
    (~sample_map: Id.Map.t(Sample.capture_spec), exp: Exp.t)
    : (Exp.t, Sample.Map.t) => {
  let elaborated = elaborate(exp);
  let (result, state) =
    Evaluator.evaluate(
      ~eval_info=EvalInfo.of_targets(sample_map),
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
