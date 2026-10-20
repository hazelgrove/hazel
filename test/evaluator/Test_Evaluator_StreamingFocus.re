open Alcotest;
open Language;
open Test_Evaluator_Prelude;

/* Probe upkeep while an evaluation streams. The worker evaluates in slices,
 * and until the last one each probe's samples stop wherever the evaluator
 * has got to. Pins and the sample focus are judged on the finished result
 * only (ProbeFocus.editor_effects ~complete). These replay a real yielding
 * evaluation slice by slice, materialized as EvalResult does. */

type streamed = {
  z: Haz3lcore.Zipper.t,
  syntax: Haz3lcore.CachedSyntax.t,
  info_map: Statics.Map.t,
  /* what the editor is given after each slice */
  partials: list(Dynamics.Map.t),
  final: Dynamics.Map.t,
};

let dynamics_of = (state: EvaluatorState.t): Dynamics.Map.t =>
  Sample.Map.finalize(EvaluatorState.get_probes(state));

let stream = (~step_budget=200, code: string): streamed => {
  let z =
    switch (Haz3lcore.Parser.to_zipper(~root=Exp, code)) {
    | Some(z) => z
    | None => fail("Failed to parse: " ++ code)
    };
  let term = Haz3lcore.MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
  let (info_map, elab) =
    Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), term);
  let syntax = Haz3lcore.CachedSyntax.mk(~info_map, ~dyn_map=Id.Map.empty, z);
  let eval_info =
    EvalInfo.of_info_map(
      ~probe_all=false,
      ~targets=targets_of_zipper(z, info_map),
      info_map,
    );
  let rec go = (evaluation, outbox, partials) =>
    switch (Evaluator.run_yielding_slice(~step_budget, evaluation)) {
    | EvaluationCompleted((_, state)) => (
        List.rev(partials),
        dynamics_of(state),
      )
    | EvaluationYielded(evaluation) =>
      let outbox =
        IncrEval.merge_outbox(
          Evaluator.drain_streaming_outbox(evaluation),
          outbox,
        );
      let partial =
        dynamics_of(StreamCollector.collect_stream_state(outbox, elab));
      go(evaluation, outbox, [partial, ...partials]);
    };
  let (partials, final) =
    go(
      Evaluator.start_yielding_evaluation(
        ~eval_info,
        ~env=Builtins.env_init,
        elab,
      ),
      IncrEval.empty_outbox,
      [],
    );
  {
    z,
    syntax,
    info_map,
    partials,
    final,
  };
};

let samples_of = (dynamics: Dynamics.Map.t, id: Id.t): list(Sample.t) =>
  Dynamics.Map.lookup(id, dynamics) |> Option.value(~default=[]);

/* The manual probes, ordered by how deep their samples' stacks run. */
let probes_by_depth = (s: streamed): list(Id.t) => {
  let depth = id =>
    List.fold_left(
      (acc, x: Sample.t) => max(acc, List.length(x.call_stack)),
      0,
      samples_of(s.final, id),
    );
  s.z.refractors.manuals
  |> List.map(fst)
  |> List.sort((a, b) => compare(depth(a), depth(b)));
};

let upkeep = (~complete, ~dynamics, s: streamed, z: Haz3lcore.Zipper.t) =>
  Haz3lcore.ProbeFocus.editor_effects(
    ~is_edited=false,
    ~complete,
    ~syntax=s.syntax,
    ~info_map=s.info_map,
    ~dynamics,
    z,
  );

let pinned = (z: Haz3lcore.Zipper.t) =>
  z.refractors.sample_focus.pinned_stack;

/* The first partial result that is not empty but would drop the pin. */
let partial_without_pin = (s: streamed, z: Haz3lcore.Zipper.t) =>
  switch (
    List.find_opt(
      dynamics =>
        !Id.Map.is_empty(dynamics)
        && pinned(Haz3lcore.ProbeFocus.drop_dead_pin(~dynamics, z)) == None,
      s.partials,
    )
  ) {
  | Some(dynamics) => dynamics
  | None => fail("no partial result lacks the pinned call")
  };

/* The call made by a probed application, as ProbeProj frames it. */
let frame_of = (ap_id: Id.t, sample: Sample.t): CallStack.frame => {
  id: ap_id,
  name: Option.bind(sample.frame, (f: CallStack.frame) => f.name),
  fn_def_id: Option.bind(sample.frame, (f: CallStack.frame) => f.fn_def_id),
};

let step_into = (s: streamed, ap_id: Id.t, sample: Sample.t) =>
  switch (
    Haz3lcore.ProbePerform.step_into_call_stack(
      ~syntax=s.syntax,
      ~call_stack=sample.call_stack,
      ~frame=frame_of(ap_id, sample),
      s.info_map,
      s.z,
    )
  ) {
  | Some(z) => z
  | None => fail("step into failed")
  };

/* What a probe shows in One mode under z's focus and pin. */
let shown = (dynamics, z: Haz3lcore.Zipper.t, id: Id.t): option(Sample.t) => {
  let focus = z.refractors.sample_focus;
  let samples =
    Sample.Selection.filter_by_pin(
      ~ap_id=None,
      ~pinned=focus.pinned_stack,
      samples_of(dynamics, id),
    );
  Sample.Selection.most_aligned_index(~ap_id=None, focus, samples)
  |> Option.map(List.nth(samples));
};

let value_of = (sample: option(Sample.t)): string =>
  switch (sample) {
  | Some(s) => Test_Evaluator_Probes.format_sample_value(s.value)
  | None => "⊖"
  };

let tests = (
  "Evaluator.StreamingFocus",
  [
    /* Watering Timer: pin a call from the last test, add a probe, and the
       pin was gone once the re-run streamed. */
    test_case(
      "A pin through map survives a streamed re-run",
      `Quick,
      () => {
        let s =
          stream(
            {|let format = fun t -> ^^probe(t * 10) in
let schedule = fun ts -> map(ts, fun t -> ^^probe(format(t))) in
let early = schedule([1, 2]) in
let late = schedule([3, 4, 5]) in
late|},
          );
        let (call_id, body_id) =
          switch (probes_by_depth(s)) {
          | [call_id, body_id] => (call_id, body_id)
          | _ => fail("expected two probes")
          };
        let last = List.nth(samples_of(s.final, call_id), 4);
        let pin_stack = [frame_of(call_id, last), ...last.call_stack];
        let z =
          Haz3lcore.SampleFocusPerform.toggle_pin_call(s.z, pin_stack, None);
        let partial = partial_without_pin(s, z);
        check(
          bool,
          "a partial result judged as finished drops the pin",
          true,
          pinned(upkeep(~complete=true, ~dynamics=partial, s, z)) == None,
        );
        let z = upkeep(~complete=false, ~dynamics=partial, s, z);
        check(
          bool,
          "while streaming the pin stays",
          true,
          pinned(z) == Some(pin_stack),
        );
        let z = upkeep(~complete=true, ~dynamics=s.final, s, z);
        check(
          bool,
          "on the result the pin stays",
          true,
          pinned(z) == Some(pin_stack),
        );
        check(
          string,
          "the body shows the pinned call",
          "50",
          value_of(shown(s.final, z, body_id)),
        );
      },
    ),
    /* Growth Plotter: step into update's 4th call (made by fold_left) and
       the re-run landed on its first call. */
    test_case(
      "Step into a call made by fold_left survives a streamed re-run",
      `Quick,
      () => {
        let s =
          stream(
            {|let update = fun (m, a) -> ^^probe(m + a) in
let run = fun (m, xs) -> fold_left(xs, fun (m, a) -> ^^probe(update(m, a)), m) in
let early = run(0, [1, 2]) in
run(early, [30, 40, 50])|},
          );
        let (call_id, body_id) =
          switch (probes_by_depth(s)) {
          | [call_id, body_id] => (call_id, body_id)
          | _ => fail("expected two probes")
          };
        let fourth = List.nth(samples_of(s.final, call_id), 3);
        let z = step_into(s, call_id, fourth);
        let stepped = pinned(z);
        let partial = partial_without_pin(s, z);
        check(
          bool,
          "a partial result judged as finished drops the step",
          true,
          pinned(upkeep(~complete=true, ~dynamics=partial, s, z)) == None,
        );
        let z = upkeep(~complete=false, ~dynamics=partial, s, z);
        check(
          bool,
          "while streaming the step stays",
          true,
          pinned(z) == stepped,
        );
        let z = upkeep(~complete=true, ~dynamics=s.final, s, z);
        check(
          bool,
          "on the result the step stays",
          true,
          pinned(z) == stepped,
        );
        check(
          string,
          "the body shows the stepped-into call",
          "73",
          value_of(shown(s.final, z, body_id)),
        );
      },
    ),
    /* Growth Plotter's first step: inside a stepped-into call, a new
       probe's focus request picked from the samples streamed so far, all
       from an earlier call. */
    test_case(
      "A new probe's focus waits for the result",
      `Quick,
      () => {
        let s =
          stream(
            {|let update = fun (m, a) -> m + a in
let run = fun (m, xs) -> fold_left(xs, fun (m, a) -> ^^probe(update(m, a)), m) in
let early = run(0, [1, 2]) in
^^probe(run(early, [30, 40, 50]))|},
          );
        let (run_id, call_id) =
          switch (probes_by_depth(s)) {
          | [run_id, call_id] => (run_id, call_id)
          | _ => fail("expected two probes")
          };
        let z = step_into(s, run_id, List.hd(samples_of(s.final, run_id)));
        let z = Haz3lcore.ProbePerform.set_pending_probe([call_id], z);
        /* streamed so far: update's calls from `early` only */
        let partial =
          switch (
            List.find_opt(
              d => {
                let calls = samples_of(d, call_id);
                calls != [] && List.length(calls) <= 2;
              },
              s.partials,
            )
          ) {
          | Some(d) => d
          | None => fail("no partial result with only early calls")
          };
        let z' = upkeep(~complete=false, ~dynamics=partial, s, z);
        check(
          bool,
          "while streaming the request waits",
          true,
          z'.refractors.pending_probe_cursor != None,
        );
        let z' = upkeep(~complete=true, ~dynamics=s.final, s, z');
        check(
          bool,
          "on the result the request resolves",
          true,
          z'.refractors.pending_probe_cursor == None,
        );
        check(
          string,
          "the focus lands on a call inside the stepped-into run",
          "33",
          value_of(shown(s.final, z', call_id)),
        );
      },
    ),
  ],
);
