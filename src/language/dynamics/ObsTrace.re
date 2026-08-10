open Util;

/* Observation trace: the evaluator narrates what happened as a sequence
 * of events, and probe samples are a fold over it (slice 3 of
 * plans/observation-trace.md: the fold IS the minting path — the state's
 * probe map is maintained incrementally by `fold_step` as events are
 * recorded, and the batch `assemble` is the same transition replayed,
 * pinned equal by Test_ObsTraceShadow).
 *
 * Representation rules:
 * - Events reference IDENTITIES (syntax id + call-stack instance), never
 *   absolute trace positions (parallelism-proofing, design §9). Closes
 *   pair with opens by bracket structure.
 * - Enter-data (arg + call frame, observable only when the Ap steps)
 *   attaches to the matching OPEN span, so its lifetime is bracket-scoped
 *   — no side table. The delegation law (at most one open span per
 *   (id, stack) key; see Evaluator.eval_3) makes the match unambiguous.
 *
 * Events are transient evaluation data: carried on EvaluatorState,
 * merged by `append` (segments splice at top-level boundaries with
 * balanced brackets), cleared by `clear_transient` before
 * serialization. */

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
  /* Point observations (pattern probes, prints) arrive pre-assembled. */
  | Minted(Sample.t);

/* An in-flight observation span: pushed at SpanOpen, enriched by a
 * matching CallEnter, popped and minted at SpanClose. */
[@deriving (show({with_path: false}), sexp, yojson)]
type open_span = {
  syntax_id: Id.t,
  stack: CallStack.t,
  step_open: int,
  app: option((CallStack.elided_value, CallStack.frame)),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type fold = {
  probes: Sample.Map.t,
  opens: list(open_span),
};

let init: fold = {
  probes: Sample.Map.empty,
  opens: [],
};

let add_sample = (probes, sample: Sample.t) =>
  Sample.Map.dominated(sample, probes)
    ? probes : Sample.Map.extend(sample.syntax_id, sample, probes);

/* The single fold transition, used incrementally by
 * EvaluatorState.record_event (the live minting path) and replayed by
 * `assemble` (tests, retention-mode queries). */
let fold_step = (acc: fold, ev: event): fold =>
  switch (ev) {
  | SpanOpen({syntax_id, stack, step}) => {
      ...acc,
      opens: [
        {
          syntax_id,
          stack,
          step_open: step,
          app: None,
        },
        ...acc.opens,
      ],
    }
  | CallEnter({frame, arg, stack}) =>
    /* Attach to the open span this enter belongs to: same id, same
     * call-stack instance. Unique if present (delegation law), and
     * usually at or near the head (the entered ap's own span), so stop
     * at the first match and share the tail. A later enter for the same
     * span overwrites (matching the old most-recent lookup semantics). */
    let stack_ids = CallStack.ids_of_stack(stack);
    let rec attach = (opens: list(open_span)) =>
      switch (opens) {
      | [] => []
      | [o, ...rest]
          when
            o.syntax_id == frame.id
            && CallStack.ids_of_stack(o.stack) == stack_ids => [
          {
            ...o,
            app: Some((arg, frame)),
          },
          ...rest,
        ]
      | [o, ...rest] => [o, ...attach(rest)]
      };
    {
      ...acc,
      opens: attach(acc.opens),
    };
  | SpanClose({syntax_id, value, env, spec, step: step_end}) =>
    let (opened, opens) =
      switch (acc.opens) {
      | [o, ...rest] when o.syntax_id == syntax_id => (o, rest)
      | _ =>
        failwith(
          "ObsTrace.fold_step: unbalanced span close for "
          ++ Id.to_string(syntax_id),
        )
      };
    let sample =
      Sample.mk(
        ~args=Option.map(fst, opened.app),
        ~frame=Option.map(snd, opened.app),
        ~step_start=opened.step_open,
        ~step_end,
        syntax_id,
        value,
        env,
        opened.stack,
        spec,
      );
    {
      probes: add_sample(acc.probes, sample),
      opens,
    };
  | Minted(sample) => {
      ...acc,
      probes: add_sample(acc.probes, sample),
    }
  };

/* Batch-assemble a probe map from a chronological event sequence. */
let assemble = (events: list(event)): Sample.Map.t => {
  let final = List.fold_left(fold_step, init, events);
  switch (final.opens) {
  | [] => final.probes
  | _ => failwith("ObsTrace.assemble: spans left open at end of trace")
  };
};
