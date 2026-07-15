open Language;
open Util;

let statics_and_elab = (exp: Exp.t): (Statics.Map.t, Exp.t) =>
  Statics.mk(
    CoreSettings.on,
    Builtins.ctx_init(Some(Operators.default_mode)),
    exp,
  );

let statics_of = (exp: Exp.t): Statics.Map.t => fst(statics_and_elab(exp));

let elaborate = (exp: Exp.t): Exp.t => snd(statics_and_elab(exp));

let evaluate = (exp: Exp.t): Exp.t => {
  let (result, _) =
    Evaluator.evaluate(~env=Builtins.env_init, elaborate(exp));
  result;
};

let evaluate_incremental =
    (~prev: IncrEval.t=IncrEval.empty, exp: Exp.t): (Exp.t, IncrEval.t) => {
  let (info_map, elab) = statics_and_elab(exp);
  let info_map =
    EvalInfoMap.of_info_map(~probe_all=CoreSettings.on.probe_all, info_map);
  let (result, state) =
    Evaluator.evaluate(~prev, ~info_map, ~env=Builtins.env_init, elab);
  (result, state.incr_eval);
};

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

/* Evaluate with a target sample_map, returning everything the `check`
 * command needs from a single run: test results, the probe samples, and the
 * type instantiations. The latter two feed live-typing refinement (a second
 * statics pass with these runtime observations as `~dynamics`). */
let evaluate_full =
    (~sample_map: Id.Map.t(Sample.capture_spec), exp: Exp.t)
    : (TestResults.t, Sample.Map.t, Dynamics.TypeInstMap.t) => {
  let (_, state) =
    Evaluator.evaluate(
      ~targets=sample_map,
      ~env=Builtins.env_init,
      elaborate(exp),
    );
  (
    TestResults.mk_results(EvaluatorState.get_tests(state)),
    EvaluatorState.get_probes(state),
    EvaluatorState.get_type_insts(state),
  );
};
