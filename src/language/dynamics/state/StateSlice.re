open Util;

/* Captures the additive side-effects that a subtree's evaluation contributes to an EvaluatorState */

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  /* `origin` is the step_count at the moment of capture (used to shift probe step_start/end
   * when replaying at a later step_count) */
  origin: int,
  steps: int,
  probes: Sample.Map.t,
  tests: list((Id.t, list(TestMap.instance_report))),
  theorems: list((Id.t, string, Environment.t(Exp.t), Exp.t)),
};

let empty: t = {
  origin: 0,
  steps: 0,
  probes: Sample.Map.empty,
  tests: [],
  theorems: [],
};

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

let diff_app_args =
    (
      ~before: Id.Map.t(list((CallStack.t, Sample.Env.elided_value))),
      ~after: Id.Map.t(list((CallStack.t, Sample.Env.elided_value))),
    )
    : Id.Map.t(list((CallStack.t, Sample.Env.elided_value))) =>
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

let shift_sample = (delta: int, s: Sample.t): Sample.t => {
  ...s,
  step_start: s.step_start + delta,
  step_end: s.step_end + delta,
};
