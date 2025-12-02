open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  theorems: list((Id.t, string, Environment.t(Exp.t), Exp.t)),
  tests: TestMap.t,
  probes: Sample.Map.t,
  type_insts: Dynamics.TypeInstMap.t,
};

type effect =
  | RecordTest(TestMap.instance_report)
  | RecordExpProbe(Probe.t)
  | RecordStackFrame
  | RecordPatProbes(PatternMatch.sample_closures)
  | RecordPrint(DHExp.t)
  | RecordTypeInstantiation(Probe.call_stack => Dynamics.TypeInstantiation.t) /* Println for probes study */
  | RecordTheorem(Id.t, string, Environment.t(Exp.t), Exp.t);

let init = {
  tests: TestMap.empty,
  probes: Sample.Map.empty,
  type_insts: Dynamics.TypeInstMap.empty,
  theorems: [],
};

let get_tests = ({tests, _}) => tests;

let get_probes = ({probes, _}) => probes;

let get_type_insts = ({type_insts, _}) => type_insts;
let get_theorems = ({theorems, _}) => theorems;

let add_test = (state: t, instance_report: TestMap.instance_report) => {
  ...state,
  tests:
    TestMap.extend(
      (DHExp.rep_id(instance_report.exp), instance_report),
      state.tests,
    ),
};
let add_sample = (state: t, sample: Sample.t) => {
  ...state,
  probes: Sample.Map.extend(sample.syntax_id, sample, state.probes),
};

let add_type_inst = (state: t, inst: Dynamics.TypeInstantiation.t) => {
  ...state,
  type_insts:
    Dynamics.TypeInstMap.extend(inst.tpat_id, inst, state.type_insts),
};
let add_theorem = ({theorems, _} as es, id, name, env, goal) => {
  {
    ...es,
    theorems: theorems |> List.append([(id, name, env, goal)]),
  };
};

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
    ((call_stack: Probe.call_stack, state: t), effect: effect) =>
      switch (effect) {
      | RecordStackFrame => ([DHExp.rep_id(init), ...call_stack], state)
      | RecordTest(instance_report) => (
          call_stack,
          add_test(state, instance_report),
        )
      | RecordExpProbe(pr) =>
        let id = DHExp.rep_id(init);
        let sample = Sample.mk(id, next, env, call_stack, pr);
        (call_stack, add_sample(state, sample));
      | RecordPatProbes(sample_closures) =>
        let state =
          List.fold_left(
            (state, sample_closure) =>
              add_sample(state, sample_closure(call_stack)),
            state,
            sample_closures,
          );
        (call_stack, state);
      | RecordPrint(value) =>
        let sample =
          Sample.mk(
            ~origin=Sample.Print,
            DHExp.rep_id(init),
            value,
            env,
            call_stack,
            Probe.empty,
          );
        (call_stack, add_sample(state, sample));
      | RecordTypeInstantiation(type_inst_closure) => (
          call_stack,
          add_type_inst(state, type_inst_closure(call_stack)),
        )
      | RecordTheorem(id, name, env, goal) => (
          call_stack,
          add_theorem(state, id, name, env, goal),
        )
      },
    (call_stack, state),
    side_effects,
  );
