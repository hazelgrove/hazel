open Util;

/* A StateSlice captures the additive side-effects that a subtree's evaluation
 * contributes to an EvaluatorState: probe samples, tests, theorems, app_args,
 * and the step_count delta. Used by the incremental evaluator: when a subtree
 * is reused from the cache, its StateSlice is replayed into the running state
 * so the UI sees identical output to a fresh run. */

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  /* step_count at the moment of capture (used to shift probe step_start/end
   * when replaying at a later step_count) */
  origin: int,
  /* total steps executed by the subtree's evaluation */
  steps: int,
  /* new probe samples keyed by syntax id, with original absolute step bounds */
  probes: Sample.Map.t,
  /* new test entries contributed (each entry is (id, new_reports_appended)) */
  tests: list((Id.t, list(TestMap.instance_report))),
  /* new theorem entries, appended in order */
  theorems: list((Id.t, string, Environment.t(Exp.t), Exp.t)),
  /* new app-arg entries per app_id, prepended in capture order */
  app_args: Id.Map.t(list((Sample.call_stack, Sample.Env.elided_value))),
};

let empty: t = {
  origin: 0,
  steps: 0,
  probes: Sample.Map.empty,
  tests: [],
  theorems: [],
  app_args: Id.Map.empty,
};

/* Helper: diff two Sample.Maps. Since samples are prepended via Sample.Map.extend,
 * after[id] consists of (new samples) @ (before[id]). So new = take(after.len - before.len, after). */
let diff_probes = (~before: Sample.Map.t, ~after: Sample.Map.t): Sample.Map.t =>
  Id.Map.fold(
    (id, after_samples, acc) => {
      let before_count =
        switch (Id.Map.find_opt(id, before)) {
        | Some(l) => List.length(l)
        | None => 0
        };
      let after_count = List.length(after_samples);
      let new_count = after_count - before_count;
      if (new_count > 0) {
        /* Take the first new_count elements (the newly prepended samples) */
        let new_samples =
          List.filteri((i, _) => i < new_count, after_samples);
        Id.Map.add(id, new_samples, acc);
      } else {
        acc;
      };
    },
    after,
    Id.Map.empty,
  );

/* Helper: diff TestMaps. Each id's list grows by appending. */
let diff_tests =
    (~before: TestMap.t, ~after: TestMap.t)
    : list((Id.t, list(TestMap.instance_report))) => {
  List.filter_map(
    ((id, after_reports)) => {
      let before_reports =
        switch (TestMap.lookup(id, before)) {
        | Some(r) => r
        | None => []
        };
      let before_count = List.length(before_reports);
      let after_count = List.length(after_reports);
      if (after_count > before_count) {
        let new_reports =
          List.filteri((i, _) => i >= before_count, after_reports);
        Some((id, new_reports));
      } else {
        None;
      };
    },
    after,
  );
};

/* Helper: diff theorems (appended list). */
let diff_theorems =
    (
      ~before: list((Id.t, string, Environment.t(Exp.t), Exp.t)),
      ~after: list((Id.t, string, Environment.t(Exp.t), Exp.t)),
    )
    : list((Id.t, string, Environment.t(Exp.t), Exp.t)) => {
  let before_len = List.length(before);
  let after_len = List.length(after);
  if (after_len > before_len) {
    List.filteri((i, _) => i >= before_len, after);
  } else {
    [];
  };
};

/* Helper: diff app_args. Each app_id's list grows by prepending. */
let diff_app_args =
    (
      ~before: Id.Map.t(list((Sample.call_stack, Sample.Env.elided_value))),
      ~after: Id.Map.t(list((Sample.call_stack, Sample.Env.elided_value))),
    )
    : Id.Map.t(list((Sample.call_stack, Sample.Env.elided_value))) =>
  Id.Map.fold(
    (id, after_entries, acc) => {
      let before_count =
        switch (Id.Map.find_opt(id, before)) {
        | Some(l) => List.length(l)
        | None => 0
        };
      let after_count = List.length(after_entries);
      let new_count = after_count - before_count;
      if (new_count > 0) {
        let new_entries =
          List.filteri((i, _) => i < new_count, after_entries);
        Id.Map.add(id, new_entries, acc);
      } else {
        acc;
      };
    },
    after,
    Id.Map.empty,
  );

/* Shift a sample's step_start/step_end by `delta`. */
let shift_sample = (delta: int, s: Sample.t): Sample.t => {
  ...s,
  step_start: s.step_start + delta,
  step_end: s.step_end + delta,
};
