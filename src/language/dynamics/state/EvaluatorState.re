[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  tests: TestMap.t,
  probes: Dynamics.Probe.Map.t,
};

type effect =
  | RecordTest(TestMap.instance_report)
  | RecordExpProbe(Probe.t)
  | RecordStackFrame
  | RecordPatProbes(PatternMatch.closure_closures);

let init = {
  tests: TestMap.empty,
  probes: Dynamics.Probe.Map.empty,
};

let get_tests = ({tests, _}) => tests;

let get_probes = ({probes, _}) => probes;

let add_test = (state: t, instance_report: TestMap.instance_report) => {
  ...state,
  tests:
    TestMap.extend(
      (DHExp.rep_id(instance_report.exp), instance_report),
      state.tests,
    ),
};
let add_closure = (state: t, closure: Dynamics.Probe.Closure.t) => {
  ...state,
  probes: Dynamics.Probe.Map.extend(closure.syntax_id, closure, state.probes),
};

let update =
    (
      state: t,
      call_stack: list(Id.t),
      env: Environment.t(Exp.t),
      ty_env: Environment.t(Typ.t),
      init: DHExp.t,
      next: DHExp.t,
      side_effects: list(effect),
    ) =>
  List.fold_left(
    ((call_stack: Probe.call_stack, state: t), effect: effect) =>
      switch (effect) {
      | RecordStackFrame => ([DHExp.rep_id(init), ...call_stack], state)
      | RecordTest(instance_report) => (
          call_stack,
          add_test(state, instance_report),
        )
      | RecordExpProbe(pr) =>
        let id = DHExp.rep_id(init);
        // print_endline(
        //   "EvaluatorStateEnv: "
        //   ++ [%derive.show: list(Environment.binding(Exp.t))](
        //        Environment.to_bindings(env),
        //      ),
        // );

        // print_endline(
        //   "EvaluatorStateTEnv: "
        //   ++ [%derive.show: list(Environment.binding(Typ.t))](
        //        Environment.to_bindings(ty_env),
        //      ),
        // );
        let closure =
          Dynamics.Probe.Closure.mk(
            ~source="EvaluatorState",
            id,
            next,
            env,
            ty_env,
            call_stack,
            pr,
          );
        (call_stack, add_closure(state, closure));
      | RecordPatProbes(closure_closures) =>
        let state =
          List.fold_left(
            (state, closure_closure) =>
              add_closure(state, closure_closure(call_stack)),
            state,
            closure_closures,
          );
        (call_stack, state);
      },
    (call_stack, state),
    side_effects,
  );
