open Util;

/*
   _____            _             _               ____  _        _
  | ____|_   ____ _| |_   _  __ _| |_ ___  _ __  / ___|| |_ __ _| |_ ___
  |  _| \ \ / / _` | | | | |/ _` | __/ _ \| '__| \___ \| __/ _` | __/ _ \
  | |___ \ V / (_| | | |_| | (_| | || (_) | |     ___) | || (_| | ||  __/
  |_____| \_/ \__,_|_|\__,_|\__,_|\__\___/|_|    |____/ \__\__,_|\__\___|

 Hazel is a PURE LANGUAGE, there is NO STATE, NOTHING TO SEE HERE, PLEASE MOVE ALONG.

 Ok so we have some state but it is all WRITE-ONLY** during evaluation, so it's
 essentially just a log we can use to query what happened during evaluation.

 ** Technically actually is't not write-only, we do read from it, but ONLY to get
 the current step count in order to record information in this state, not to affect
 evaluation in any way. You'll notice that this step count thing is what requires
 most of the work in appending states.
 */

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  initial_step_count: int,
  theorems: list((Id.t, string, Environment.t(Exp.t), Exp.t)),
  tests: TestMap.t,
  probes: Sample.Map.t,
  /* Transient enter-event data for probed applications (args + call
   * frame, observable only when the Ap steps). Consumed when the probe's
   * observation span closes in Evaluator.eval_3; carried up through
   * sub-evaluations by `append`. Cleared by `clear_transient` before the
   * state leaves the evaluator (postMessage / cache entries). */
  app_data: CallStack.app_data,
  /* Shadow observation trace (plans/observation-trace.md slice 2):
   * events emitted alongside inline sample minting; ObsTrace.assemble
   * over the (reversed) list must reproduce `probes` exactly. Transient:
   * merged by append, cleared by clear_transient. Prepend order
   * (newest first), like every other accumulator here. */
  obs_trace: list(ObsTrace.event),
  step_count: int,
  incr_eval,
}

// Note[Matt]: There are probably memory improvements to be made here by untying this knot.
and incr_eval = IncrEval.t(t);

type effect =
  | RecordTest(TestMap.instance_report)
  | RecordStackFrame(option(string), option(DHExp.t), option(Id.t)) /* (fn_name, arg_value, fn_def_id) */
  /* A pattern was matched against a value during evaluation. Carries the
   * pat and rhs so the incremental evaluator can decide which body-scoped
   * names became dirty, and any probe samples produced by the match. */
  | RecordPatMatch({
      pat: Pat.t,
      rhs: DHExp.t,
      samples: PatternMatch.sample_closures,
    })
  | RecordTheorem(Id.t, string, Environment.t(Exp.t), Exp.t)
  | RecordPrint(DHExp.t); /* Println for probes study */

let empty: t = {
  initial_step_count: 0,
  tests: TestMap.empty,
  probes: Sample.Map.empty,
  app_data: Id.Map.empty,
  obs_trace: [],
  step_count: 0,
  theorems: [],
  incr_eval: IncrEval.empty,
};

let empty_at = (step_count: int): t => {
  ...empty,
  initial_step_count: step_count,
  step_count,
};

let get_step_count = ({step_count, _}: t): int => step_count;

let shift_sample = (delta: int, s: Sample.t): Sample.t => {
  ...s,
  step_start: s.step_start + delta,
  step_end: s.step_end + delta,
};

/* Merge `ext` into `base`, shifting probe step bounds when the timelines
 * don't line up (base.step_count vs ext.initial_step_count). */
let append = (base: t, ext: t): t => {
  let delta = base.step_count - ext.initial_step_count;
  let probes =
    Id.Map.fold(
      (id, ext_samples, acc) => {
        let samples =
          if (delta == 0) {
            ext_samples;
          } else {
            List.map(shift_sample(delta), ext_samples);
          };
        let existing =
          switch (Id.Map.find_opt(id, acc)) {
          | Some(l) => l
          | None => []
          };
        Id.Map.add(id, samples @ existing, acc);
      },
      ext.probes,
      base.probes,
    );
  let tests =
    List.fold_left(
      (acc, (id, reports)) =>
        List.fold_left(
          (acc, report) => TestMap.extend((id, report), acc),
          acc,
          reports,
        ),
      base.tests,
      ext.tests,
    );
  let app_data =
    if (Id.Map.is_empty(ext.app_data)) {
      base.app_data;
    } else {
      Id.Map.fold(
        (id, entries, acc) => {
          let existing =
            Id.Map.find_opt(id, acc) |> Option.value(~default=[]);
          Id.Map.add(id, entries @ existing, acc);
        },
        ext.app_data,
        base.app_data,
      );
    };
  let obs_trace =
    switch (ext.obs_trace) {
    | [] => base.obs_trace
    | ext_events => ext_events @ base.obs_trace
    };
  {
    ...base,
    step_count: base.step_count + (ext.step_count - ext.initial_step_count),
    probes,
    tests,
    theorems: ext.theorems @ base.theorems,
    app_data,
    obs_trace,
  };
};

/* Restart a state's timeline at step 0, shifting its probe step bounds
 * accordingly (used when replaying cached/streamed states). */
let rebase = (ext: t): t => append(empty, ext);

/* Drop data only needed while evaluation is in flight. Call before the
 * state leaves the evaluator (postMessage, cache/outbox entries) to avoid
 * serializing arg values, which can be large. */
let clear_transient = (state: t): t => {
  ...state,
  app_data: Id.Map.empty,
  obs_trace: [],
};

let record_event = (state: t, ev: ObsTrace.event): t => {
  ...state,
  obs_trace: [ev, ...state.obs_trace],
};

let get_tests = ({tests, _}) => tests;

let get_probes = ({probes, _}) => probes;

let get_theorems = ({theorems, _}) => theorems;

let get_incr_eval = ({incr_eval, _}: t) => incr_eval;

let add_incr_entry = (state: t, id: Id.t, entry: IncrEval.entry(t)): t => {
  ...state,
  incr_eval: IncrEval.add_entry(id, entry, state.incr_eval),
};

let add_sample = (state: t, sample: Sample.t) =>
  /* Dedup via the shared ascription-dominance predicate (see
   * Sample.Map.dominated; also used by ObsTrace.assemble so shadow
   * parity is exact). Historical note: a same-context dedup rule for
   * wrap_closure_when_done duplicates was removed after the root cause
   * was fixed (is_value=true prevents re-evaluation). */
  if (Sample.Map.dominated(sample, state.probes)) {
    state;
  } else {
    {
      ...state,
      probes: Sample.Map.extend(sample.syntax_id, sample, state.probes),
    };
  };

let update =
    (
      eval_info: EvalInfo.t,
      state: t,
      call_stack: CallStack.t,
      env: Environment.t(Exp.t),
      init: DHExp.t,
      side_effects: list(effect),
    )
    : (CallStack.t, t) => {
  /* Elide arg value for storage (handles closures, etc.) */
  let elide_arg =
      (env: Environment.t(Exp.t), d: DHExp.t): Sample.Env.elided_value =>
    Sample.Env.elide(env, d);

  let add_test = (state: t, instance_report: TestMap.instance_report) => {
    ...state,
    tests:
      TestMap.extend(
        (DHExp.rep_id(instance_report.exp), instance_report),
        state.tests,
      ),
  };

  let add_theorem = ({theorems, _} as es, id, name, env, goal) => {
    {
      ...es,
      theorems: theorems |> List.append([(id, name, env, goal)]),
    };
  };

  /* Increment step count for this evaluation step */
  let state = {
    ...state,
    step_count: state.step_count + 1,
  };

  List.fold_left(
    ((call_stack: CallStack.t, state: t), effect: effect) =>
      switch (effect) {
      | RecordStackFrame(fn_name, arg_opt, fn_def_id) =>
        let app_id = DHExp.rep_id(init);
        let frame: CallStack.frame = {
          id: app_id,
          name: fn_name,
          fn_def_id,
        };
        /* Only store data for probe-target app_ids, else app_data balloons
         * for programs with many calls but no probes on them. */
        let state =
          switch (arg_opt) {
          | Some(arg) when Id.Map.mem(app_id, eval_info.targets) =>
            let elided_arg = elide_arg(env, arg);
            let state =
              record_event(
                state,
                ObsTrace.CallEnter({
                  frame,
                  arg: elided_arg,
                  stack: call_stack,
                }),
              );
            {
              ...state,
              app_data:
                CallStack.add_app_data(
                  state.app_data,
                  ~stack=call_stack,
                  app_id,
                  elided_arg,
                  frame,
                ),
            };
          | Some(_)
          | None => state
          };
        (CallStack.add_entry(call_stack, frame), state);
      | RecordTest(instance_report) => (
          call_stack,
          add_test(state, instance_report),
        )
      | RecordPatMatch({samples: sample_closures, _}) =>
        /* Pattern probes are recorded at the current step, then we
         * increment to ensure patterns don't share step boundaries
         * with subsequent expressions (which would cause incorrect
         * containment classification in StepRange mode) */
        let step = state.step_count;
        let state =
          List.fold_left(
            (state: t, sample_closure: (CallStack.t, int, int) => Sample.t) => {
              let sample = sample_closure(call_stack, step, step);
              add_sample(
                record_event(state, ObsTrace.Minted(sample)),
                sample,
              );
            },
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
        (
          call_stack,
          add_sample(record_event(state, ObsTrace.Minted(sample)), sample),
        );
      | RecordTheorem(id, name, env, goal) => (
          call_stack,
          add_theorem(state, id, name, env, goal),
        )
      },
    (call_stack, state),
    side_effects,
  );
};
