open Util;

/* Observation trace: the evaluator narrates what happened as a sequence
 * of events; observation features are folds over it. This is slice 2 of
 * plans/observation-trace.md: events are emitted ALONGSIDE the existing
 * inline sample minting (shadow mode), and `assemble` must reproduce the
 * probe map exactly — pinned by Test_ObsTraceShadow.
 *
 * Representation rule (parallelism-proofing, design §9): events reference
 * IDENTITIES (syntax id + call-stack instance), never absolute trace
 * positions. Closes pair with opens by bracket structure. Positions are a
 * view assigned at linearization/query time, so segment splicing (append)
 * is free.
 *
 * Events are transient evaluation data: carried on EvaluatorState,
 * merged by `append` (segments splice at top-level boundaries, so
 * bracket structure survives), cleared by `clear_transient` before
 * serialization. SpanClose carries env/spec so the fold can mint
 * byte-identical samples; in slice 3 the inline minting is removed and
 * this cost transfers rather than doubles. */

[@deriving (show({with_path: false}), sexp, yojson)]
type event =
  | SpanOpen({
      syntax_id: Id.t,
      stack: CallStack.t, /* call-stack instance at open (before entry) */
      step: int,
    })
  | SpanClose({
      syntax_id: Id.t,
      value: DHExp.t,
      env: Environment.t(Exp.t),
      spec: Sample.capture_spec,
      step: int,
    })
  | CallEnter({
      frame: CallStack.frame,
      arg: CallStack.elided_value,
      stack: CallStack.t,
    }) /* stack BEFORE entering, the enter's identity */
  /* Point observations (pattern probes, prints) arrive pre-assembled for
   * now; spans are the machinery under test in this slice. */
  | Minted(Sample.t);

/* Assemble the probe map from a chronological event sequence. Mirrors the
 * inline path: samples minted at span close, args/frame looked up by
 * (app_id, stack-before-entering), dominance dedup applied on add. */
let assemble = (events: list(event)): Sample.Map.t => {
  let enter_key = (id: Id.t, stack: CallStack.t) => (
    id,
    CallStack.ids_of_stack(stack),
  );
  let add_sample = (probes, sample: Sample.t) =>
    Sample.Map.dominated(sample, probes)
      ? probes : Sample.Map.extend(sample.syntax_id, sample, probes);
  let (probes, opens, _enters) =
    List.fold_left(
      ((probes, opens, enters), ev: event) =>
        switch (ev) {
        | SpanOpen({syntax_id, stack, step}) => (
            probes,
            [(syntax_id, stack, step), ...opens],
            enters,
          )
        | CallEnter({frame, arg, stack}) => (
            probes,
            opens,
            [(enter_key(frame.id, stack), (arg, frame)), ...enters],
          )
        | SpanClose({syntax_id, value, env, spec, step: step_end}) =>
          let (open_stack, step_start, opens) =
            switch (opens) {
            | [(id, stack, step), ...rest] when id == syntax_id => (
                stack,
                step,
                rest,
              )
            | _ =>
              failwith(
                "ObsTrace.assemble: unbalanced span close for "
                ++ Id.to_string(syntax_id),
              )
            };
          let app = List.assoc_opt(enter_key(syntax_id, open_stack), enters);
          let sample =
            Sample.mk(
              ~args=Option.map(fst, app),
              ~frame=Option.map(snd, app),
              ~step_start,
              ~step_end,
              syntax_id,
              value,
              env,
              open_stack,
              spec,
            );
          (add_sample(probes, sample), opens, enters);
        | Minted(sample) => (add_sample(probes, sample), opens, enters)
        },
      (Sample.Map.empty, [], []),
      events,
    );
  switch (opens) {
  | [] => probes
  | _ => failwith("ObsTrace.assemble: spans left open at end of trace")
  };
};
