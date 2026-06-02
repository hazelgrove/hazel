open Util;

/* Argument values for function applications, keyed by app_id.
 * Each entry is a list of (call_stack_before_entering, elided_arg_value).
 * The call_stack is the stack BEFORE entering the function, so we can match
 * samples taken inside the function with their calling arguments. */
[@deriving (show({with_path: false}), sexp, yojson)]
type app_args_t =
  Id.Map.t(list((Sample.call_stack, Sample.Env.elided_value)));

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  theorems: list((Id.t, string, Environment.t(Exp.t), Exp.t)),
  tests: TestMap.t,
  probes: Sample.Map.t,
  app_args: app_args_t, /* Argument values for function applications */
  step_count: int,
  pending_probe_starts: Id.Map.t(list(int)), /* Stack per probe_id; nested recursive calls push/pop */
  targets: Sample.targets, /* IDs of expressions/patterns to sample */
  incr_eval: IncrEval.t /* Per-id cache entries and reuse/recalc bookkeeping for the incremental evaluator */
};

type effect =
  | RecordTest(TestMap.instance_report)
  | RecordExpProbe(Sample.capture_spec)
  | RecordAscriptionProbe((Id.t, Sample.capture_spec, Exp.t))
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

let mk = (~targets: Sample.targets): t => {
  tests: TestMap.empty,
  probes: Sample.Map.empty,
  app_args: Id.Map.empty,
  step_count: 0,
  pending_probe_starts: Id.Map.empty,
  targets,
  theorems: [],
  incr_eval: IncrEval.empty,
};

let init: t = mk(~targets=Sample.no_targets);

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

let get_app_args = ({app_args, _}) => app_args;

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
  app_args: Id.Map.empty,
  pending_probe_starts: Id.Map.empty,
  targets: Id.Map.empty,
};

/* Elide arg value for storage (handles closures, etc.) */
let elide_arg =
    (env: Environment.t(Exp.t), d: DHExp.t): Sample.Env.elided_value =>
  Sample.Env.elide(env, d);

/* Add an argument value for an application */
let add_app_arg =
    (
      state: t,
      app_id: Id.t,
      call_stack: Sample.call_stack,
      arg: Sample.Env.elided_value,
    )
    : t => {
  let existing =
    Id.Map.find_opt(app_id, state.app_args) |> Option.value(~default=[]);
  {
    ...state,
    app_args:
      Id.Map.add(app_id, [(call_stack, arg), ...existing], state.app_args),
  };
};

/* Look up argument value for an application at a specific call_stack.
 * Used when creating samples for probes on Ap expressions. */
let lookup_app_arg =
    (state: t, app_id: Id.t, call_stack: Sample.call_stack)
    : option(Sample.Env.elided_value) => {
  let call_stack_ids = Sample.ids_of_stack(call_stack);
  switch (Id.Map.find_opt(app_id, state.app_args)) {
  | None => None
  | Some(entries) =>
    List.find_map(
      ((stored_stack, arg)) =>
        Sample.ids_of_stack(stored_stack) == call_stack_ids
          ? Some(arg) : None,
      entries,
    )
  };
};

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
      | RecordStackFrame(fn_name, arg_opt, fn_def_id) =>
        let app_id = DHExp.rep_id(init);
        /* Only store argument value if this app_id is a probe target.
         * This avoids accumulating massive app_args data for programs
         * with many function calls but no probes on those calls. */
        let state =
          switch (arg_opt) {
          | Some(arg) when Id.Map.mem(app_id, state.targets) =>
            let elided_arg = elide_arg(env, arg);
            add_app_arg(state, app_id, call_stack, elided_arg);
          | Some(_)
          | None => state
          };
        (
          [
            {
              id: app_id,
              name: fn_name,
              fn_def_id,
            },
            ...call_stack,
          ],
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
        let args = lookup_app_arg(state, probe_id, call_stack);
        let sample =
          Sample.mk(
            ~args,
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
      | RecordAscriptionProbe((id, capture_spec, ascribed_exp)) =>
        let step = state.step_count;
        /* Substitute env so a Var body resolves to its runtime value. */
        let ascribed_exp = Substitution.in_exp(env, ascribed_exp);
        let sample =
          Sample.mk(
            ~step_start=step,
            ~step_end=step,
            id,
            ascribed_exp,
            env,
            call_stack,
            capture_spec,
          );
        let state = add_sample(state, sample);
        let state = {
          ...state,
          step_count: state.step_count + 1,
        };
        (call_stack, state);
      | RecordPatMatch({samples: sample_closures, _}) =>
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

/* Capture the delta between `before` and `after` as a StateSlice. */
let capture_slice = (~before: t, ~after: t): StateSlice.t => {
  origin: before.step_count,
  steps: after.step_count - before.step_count,
  probes: StateSlice.diff_probes(~before=before.probes, ~after=after.probes),
  tests: StateSlice.diff_tests(~before=before.tests, ~after=after.tests),
  theorems:
    StateSlice.diff_theorems(~before=before.theorems, ~after=after.theorems),
  app_args:
    StateSlice.diff_app_args(~before=before.app_args, ~after=after.app_args),
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
  let app_args =
    Id.Map.fold(
      (id, new_entries, acc) => {
        let existing =
          switch (Id.Map.find_opt(id, acc)) {
          | Some(l) => l
          | None => []
          };
        Id.Map.add(id, new_entries @ existing, acc);
      },
      slice.app_args,
      state.app_args,
    );
  {
    ...state,
    step_count: state.step_count + slice.steps,
    probes,
    tests,
    theorems,
    app_args,
  };
};
