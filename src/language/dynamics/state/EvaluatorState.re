open Util;

/* pending_probe_starts is transient state only needed during evaluation,
 * so we exclude it from serialization by making it opaque */
[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  tests: TestMap.t,
  probes: Sample.Map.t,
  step_count: int,
  [@sexp.opaque] [@yojson.opaque]
  pending_probe_starts: Id.Map.t(int),
};

type effect =
  | RecordTest(TestMap.instance_report)
  | RecordExpProbe(Probe.t)
  | RecordStackFrame
  | RecordPatProbes(PatternMatch.sample_closures)
  | RecordPrint(DHExp.t); /* Println for probes study */

let init: t = {
  tests: TestMap.empty,
  probes: Sample.Map.empty,
  step_count: 0,
  pending_probe_starts: Id.Map.empty,
};

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

let update =
    (
      state: t,
      call_stack: list(Id.t),
      env: Environment.t(Exp.t),
      init: DHExp.t,
      next: DHExp.t,
      side_effects: list(effect),
    )
    : (Probe.call_stack, t) => {
  /* Increment step count for this evaluation step */
  let state = {
    ...state,
    step_count: state.step_count + 1,
  };

  List.fold_left(
    ((call_stack: Probe.call_stack, state: t), effect: effect) =>
      switch (effect) {
      | RecordStackFrame => ([DHExp.rep_id(init), ...call_stack], state)
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
              sample_closure: (Probe.call_stack, int, int) => Sample.t,
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
            Probe.empty,
          );
        (call_stack, add_sample(state, sample));
      },
    (call_stack, state),
    side_effects,
  );
};
