open Util;

/* Semantic information gathered during evaluation. This aspirationally
 * unifies all evaluator output, in the same sense as Statics does for
 * static information gathering, but right now it specifically handles
 * sample gathering for probe projectors */

module Info = {
  /* Collected samples for a given id */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    samples: list(Sample.t),
    sample_focus: Sample.Focus.t,
  };

  let init = {
    samples: [],
    sample_focus: Sample.Focus.init,
  };

  let is_in = (di: t): option(Sample.t) => {
    let cursor_stack = Sample.Focus.effective_stack(di.sample_focus);
    List.find_opt(
      (sample: Sample.t) => CallStack.equal(sample.call_stack, cursor_stack),
      di.samples,
    );
  };

  /* Find the sample most aligned with the cursor's call path.
   * Uses the same suffix-first principle as Selection.most_aligned_index
   * but returns the sample directly. */
  let most_aligned_sample = (ap_id: option(Id.t), di: t): option(Sample.t) =>
    Sample.Selection.most_aligned_sample(
      ~ap_id,
      ~cursor=di.sample_focus,
      di.samples,
    );
};

module Map = {
  /* holds FINALIZED maps (eval order) so lookup is a plain find — see Sample.Map.finalize */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Sample.Map.t;
  let empty: t = Sample.Map.empty;
  let mk: t => t = Fun.id;
  let lookup = (id, map: t) => Id.Map.find_opt(id, map);
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  probe_map: Sample.Map.t,
  test_results: TestResults.t,
  theorems: list((Id.t, string, Environment.t(Exp.t), Exp.t)),
};
