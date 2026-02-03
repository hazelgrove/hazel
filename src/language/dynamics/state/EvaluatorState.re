open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  theorems: list((Id.t, string, Environment.t(Exp.t), Exp.t)),
  tests: TestMap.t,
  probes: Sample.Map.t,
  step_count: int,
  pending_probe_starts: Id.Map.t(int), /* Transient state only needed during evaluation */
  targets: Sample.targets /* IDs of expressions/patterns to sample */
};

type effect =
  | RecordTest(TestMap.instance_report)
  | RecordExpProbe(Sample.capture_spec)
  | RecordStackFrame(option(string)) /* Function name extracted at evaluation time */
  | RecordPatProbes(PatternMatch.sample_closures)
  | RecordTheorem(Id.t, string, Environment.t(Exp.t), Exp.t)
  | RecordPrint(DHExp.t); /* Println for probes study */

let mk = (~targets: Sample.targets): t => {
  tests: TestMap.empty,
  probes: Sample.Map.empty,
  step_count: 0,
  pending_probe_starts: Id.Map.empty,
  targets,
  theorems: [],
};

let init: t = mk(~targets=Sample.no_targets);

let get_step_count = ({step_count, _}: t): int => step_count;

let record_probe_start = (state: t, probe_id: Id.t): t => {
  ...state,
  pending_probe_starts:
    Id.Map.add(probe_id, state.step_count, state.pending_probe_starts),
};

let get_probe_start = (state: t, probe_id: Id.t): option(int) =>
  Id.Map.find_opt(probe_id, state.pending_probe_starts);

let clear_probe_start = (state: t, probe_id: Id.t): t => {
  ...state,
  pending_probe_starts: Id.Map.remove(probe_id, state.pending_probe_starts),
};

let get_tests = ({tests, _}) => tests;

let get_probes = ({probes, _}) => probes;

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

let add_theorem = ({theorems, _} as es, id, name, env, goal) => {
  {
    ...es,
    theorems: theorems |> List.append([(id, name, env, goal)]),
  };
};

let update =
    (
      state: t,
      call_stack: Sample.call_stack,
      env: Environment.t(Exp.t),
      init: DHExp.t,
      next: DHExp.t,
      side_effects: list(effect),
    )
    : (Sample.call_stack, t) => {
  /* Increment step count for this evaluation step */
  let state = {
    ...state,
    step_count: state.step_count + 1,
  };

  List.fold_left(
    ((call_stack: Sample.call_stack, state: t), effect: effect) =>
      switch (effect) {
      | RecordStackFrame(fn_name) => (
          [(DHExp.rep_id(init), fn_name), ...call_stack],
          state,
        )
      | RecordTest(instance_report) => (
          call_stack,
          add_test(state, instance_report),
        )
      | RecordExpProbe(pr) =>
        let probe_id = DHExp.rep_id(init);
        /* step_start is when we began evaluating the probe (recorded earlier)
         * step_end is step_count - 1 because this step is the "strip probe" step */
        let step_start =
          get_probe_start(state, probe_id) |> Option.value(~default=0);
        let step_end = state.step_count - 1;
        let sample =
          Sample.mk(
            ~step_start,
            ~step_end,
            probe_id,
            next,
            env,
            call_stack,
            pr,
          );
        let state = clear_probe_start(state, probe_id);
        (call_stack, add_sample(state, sample));
      | RecordPatProbes(sample_closures) =>
        /* Pattern probes are recorded at the current step, then we
         * increment to ensure patterns don't share step boundaries
         * with subsequent expressions (which would cause incorrect
         * containment classification in StepRange mode) */
        let step = state.step_count;
        let state =
          List.fold_left(
            (
              state: t,
              sample_closure: (Sample.call_stack, int, int) => Sample.t,
            ) =>
              add_sample(state, sample_closure(call_stack, step, step)),
            state,
            sample_closures,
          );
        /* Advance step count past pattern evaluation */
        let state = {
          ...state,
          step_count: state.step_count + 1,
        };
        (call_stack, state);
      | RecordPrint(value) =>
        /* Print happens in a single step */
        let step = state.step_count;
        let sample =
          Sample.mk(
            ~origin=Sample.Print,
            ~step_start=step,
            ~step_end=step,
            DHExp.rep_id(init),
            value,
            env,
            call_stack,
            Sample.empty_capture_spec,
          );
        (call_stack, add_sample(state, sample));
      | RecordTheorem(id, name, env, goal) => (
          call_stack,
          add_theorem(state, id, name, env, goal),
        )
      },
    (call_stack, state),
    side_effects,
  );
};
