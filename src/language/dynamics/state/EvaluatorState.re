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

let add_test = ({tests, _} as es, id, report) => {
  let tests = tests |> TestMap.extend((id, report));
  {
    ...es,
    tests,
  };
};

let get_tests = ({tests, _}) => tests;

let add_closure = ({probes, _} as es, closure: Dynamics.Probe.Closure.t) => {
  ...es,
  probes: Dynamics.Probe.Map.extend(closure.syntax_id, closure, probes),
};

let get_probes = ({probes, _}) => probes;

let update =
    (
      state: t,
      call_stack: list(Id.t),
      env: Environment.t(Exp.t),
      init: DHExp.t,
      next: DHExp.t,
      side_effects: list(effect),
    ) =>
  List.fold_left(
    ((call_stack, state), side_effect) =>
      switch (side_effect) {
      | RecordStackFrame => ([DHExp.rep_id(init), ...call_stack], state)
      | RecordTest(instance_report) =>
        let id = DHExp.rep_id(instance_report.exp);
        (call_stack, add_test(state, id, instance_report));
      | RecordExpProbe(pr) =>
        let id = DHExp.rep_id(init);
        let closure =
          Dynamics.Probe.Closure.mk(id, next, env, call_stack, pr);
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
