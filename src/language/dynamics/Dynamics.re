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
    /* The pinned span's step interval, resolved once at info-assembly
     * time (the pinned sample usually belongs to a DIFFERENT probe than
     * the one viewing). Lets filter_by_pin use temporal containment
     * — "during the pinned evaluation" — instead of stack-id suffixes,
     * which is also the correct semantics for pinning non-call spans. */
    pinned_interval: option((int, int)),
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
  /* The dynamics an editor shows on its terms: the probe samples, as a
     FINALIZED map (eval order) so lookup is a plain find — see
     Sample.Map.finalize — and how many times each term was evaluated. */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    samples: Sample.Map.t,
    evaluations: Id.Map.t(int),
  };
  let empty: t = {
    samples: Sample.Map.empty,
    evaluations: Id.Map.empty,
  };
  let lookup = (id, {samples, _}: t) => Id.Map.find_opt(id, samples);
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  /* one record per result, so an editor can tell a new result by identity */
  map: Map.t,
  test_results: TestResults.t,
  theorems: list((Id.t, string, Environment.t(Exp.t), Exp.t)),
};
