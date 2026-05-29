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
 evaluation in any way.
 */

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  theorems: list((Id.t, string, Environment.t(Exp.t), Exp.t)),
  tests: TestMap.t,
  probes: Sample.Map.t,
  step_count: int,
  pending_probe_starts: Id.Map.t(list(int)), /* Stack per probe_id; nested recursive calls push/pop */
  incr_eval: IncrEval.t /* Per-id cache entries and reuse/recalc bookkeeping for the incremental evaluator */
};

type effect =
  | RecordTest(TestMap.instance_report)
  | RecordExpProbe(Sample.capture_spec)
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
  tests: TestMap.empty,
  probes: Sample.Map.empty,
  step_count: 0,
  pending_probe_starts: Id.Map.empty,
  theorems: [],
  incr_eval: IncrEval.empty,
};

let get_step_count = ({step_count, _}: t): int => step_count;

let record_probe_start = (state: t, probe_id: Id.t): t => {
  let stack =
    Id.Map.find_opt(probe_id, state.pending_probe_starts)
    |> Option.value(~default=[]);
  {
    ...state,
    pending_probe_starts:
      Id.Map.add(
        probe_id,
        [state.step_count, ...stack],
        state.pending_probe_starts,
      ),
  };
};

let get_probe_start = (state: t, probe_id: Id.t): option(int) =>
  switch (Id.Map.find_opt(probe_id, state.pending_probe_starts)) {
  | Some([head, ..._]) => Some(head)
  | _ => None
  };

let clear_probe_start = (state: t, probe_id: Id.t): t => {
  let pending =
    switch (Id.Map.find_opt(probe_id, state.pending_probe_starts)) {
    | Some([_, ...rest]) when rest != [] =>
      Id.Map.add(probe_id, rest, state.pending_probe_starts)
    | _ => Id.Map.remove(probe_id, state.pending_probe_starts)
    };
  {
    ...state,
    pending_probe_starts: pending,
  };
};

let get_tests = ({tests, _}) => tests;

let get_probes = ({probes, _}) => probes;

let get_theorems = ({theorems, _}) => theorems;

let get_incr_eval = ({incr_eval, _}: t) => incr_eval;

let add_incr_entry = (state: t, id: Id.t, entry: IncrEval.entry): t => {
  ...state,
  incr_eval: IncrEval.add_entry(id, entry, state.incr_eval),
};

let mark_incr_reused = (state: t, id: Id.t): t => {
  ...state,
  incr_eval: IncrEval.mark_reused(id, state.incr_eval),
};

let mark_incr_recalculated = (state: t, id: Id.t): t => {
  ...state,
  incr_eval: IncrEval.mark_recalculated(id, state.incr_eval),
};

/* Clear transient data that's only needed during evaluation.
 * Call this before sending EvaluatorState over postMessage
 * to avoid serializing massive amounts of unnecessary data.
 * - app_args: only needed to look up args during sample creation
 * - pending_probe_starts: only needed during evaluation
 * - targets: only needed during evaluation */
let clear_transient = (state: t): t => {
  ...state,
  pending_probe_starts: Id.Map.empty,
};

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
let add_sample = (state: t, sample: Sample.t) => {
  /* Deduplicate: skip recording if an existing sample for this
   * syntax_id makes the new one redundant.
   *
   * Ascription dominance: a non-empty call_stack sample is
   * dominated by an existing empty call_stack sample. This
   * prevents duplicates from Asc distribution through typed
   * functions, where inner values get re-evaluated at deeper
   * call stacks.
   *
   * Note: previously had a same-context (equal call_stack) dedup rule
   * to handle wrap_closure_when_done re-evaluation duplicates. That rule
   * was removed because the root cause was fixed: wrap_closure_when_done
   * now uses is_value=true, so the Closure-wrapped expression is returned
   * as Final immediately without triggering re-evaluation. */
  let dominated =
    switch (Id.Map.find_opt(sample.syntax_id, state.probes)) {
    | Some(existing) =>
      List.exists(
        (s: Sample.t) => sample.call_stack != [] && s.call_stack == [],
        existing,
      )
    | None => false
    };
  if (dominated) {
    state;
  } else {
    {
      ...state,
      probes: Sample.Map.extend(sample.syntax_id, sample, state.probes),
    };
  };
};

let add_theorem = ({theorems, _} as es, id, name, env, goal) => {
  {
    ...es,
    theorems: theorems |> List.append([(id, name, env, goal)]),
  };
};

let update =
    (
      info_map: EvalInfo.t,
      state: t,
      call_stack: CallStack.t',
      env: Environment.t(Exp.t),
      init: DHExp.t,
      next: DHExp.t,
      side_effects: list(effect),
    )
    : (CallStack.t', t) => {
  /* Increment step count for this evaluation step */
  let state = {
    ...state,
    step_count: state.step_count + 1,
  };

  List.fold_left(
    ((call_stack: CallStack.t', state: t), effect: effect) =>
      switch (effect) {
      | RecordStackFrame(fn_name, arg_opt, fn_def_id) =>
        let app_id = DHExp.rep_id(init);
        /* Only store argument value if this app_id is a probe target.
         * This avoids accumulating massive app_args data for programs
         * with many function calls but no probes on those calls. */
        let call_stack =
          switch (arg_opt) {
          | Some(arg) when Id.Map.mem(app_id, info_map.targets) =>
            let elided_arg = elide_arg(env, arg);
            CallStack.add_app_arg(call_stack, app_id, elided_arg);
          | Some(_)
          | None => call_stack
          };
        (
          CallStack.add_entry(
            call_stack,
            {
              id: app_id,
              name: fn_name,
              fn_def_id,
            },
          ),
          state,
        );
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
        /* Look up arg if this probe is on an Ap expression */
        let args =
          CallStack.lookup_app_arg(call_stack, probe_id, call_stack.stack);
        let sample =
          Sample.mk(
            ~args,
            ~step_start,
            ~step_end,
            probe_id,
            next,
            env,
            call_stack.stack,
            pr,
          );
        let state = clear_probe_start(state, probe_id);
        (call_stack, add_sample(state, sample));
      | RecordPatMatch({samples: sample_closures, _}) =>
        /* Pattern probes are recorded at the current step, then we
         * increment to ensure patterns don't share step boundaries
         * with subsequent expressions (which would cause incorrect
         * containment classification in StepRange mode) */
        let step = state.step_count;
        let state =
          List.fold_left(
            (state: t, sample_closure: (CallStack.t, int, int) => Sample.t) =>
              add_sample(
                state,
                sample_closure(call_stack.stack, step, step),
              ),
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
            call_stack.stack,
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

/* Capture the delta between `before` and `after` as a StateSlice. */
let capture_slice = (~before: t, ~after: t): StateSlice.t => {
  origin: before.step_count,
  steps: after.step_count - before.step_count,
  probes: StateSlice.diff_probes(~before=before.probes, ~after=after.probes),
  tests: StateSlice.diff_tests(~before=before.tests, ~after=after.tests),
  theorems:
    StateSlice.diff_theorems(~before=before.theorems, ~after=after.theorems),
};

/* Replay a slice into `state`: add its sample/test/theorem/app_arg entries,
 * bump step_count by the slice's step delta. Probe step bounds are shifted
 * so they sit within the current step_count window. */
let replay_slice = (slice: StateSlice.t, state: t): t => {
  let delta = state.step_count - slice.origin;
  let probes =
    Id.Map.fold(
      (id, new_samples, acc) => {
        let shifted = List.map(StateSlice.shift_sample(delta), new_samples);
        let existing =
          switch (Id.Map.find_opt(id, acc)) {
          | Some(l) => l
          | None => []
          };
        Id.Map.add(id, shifted @ existing, acc);
      },
      slice.probes,
      state.probes,
    );
  let tests =
    List.fold_left(
      (acc, (id, new_reports)) =>
        List.fold_left(
          (acc, report) => TestMap.extend((id, report), acc),
          acc,
          new_reports,
        ),
      state.tests,
      slice.tests,
    );
  let theorems = state.theorems @ slice.theorems;
  {
    ...state,
    step_count: state.step_count + slice.steps,
    probes,
    tests,
    theorems,
  };
};
