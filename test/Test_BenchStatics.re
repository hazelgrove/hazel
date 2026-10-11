open Alcotest;
open Haz3lcore;
open Language;

/* statics parity gates over the mega corpus */

let parse_seg_of = CorpusUtil.seg_of(~root=Exp);

/* a probe in a fn body called only from a later item samples under
   compositional statics as under monolithic (fresh evaluations) */
let probe_capture_parity = (): unit => {
  let settings = CoreSettings.on;
  let ctx = Builtins.ctx_init(Some(Operators.default_mode));
  let term =
    MakeTerm.go(
      parse_seg_of("let f = fun q -> q + 1 in\nlet z = f(5) in\nz"),
    ).
      term;
  let (map0, _) = Statics.mk_unmemoized(settings, ctx, term);
  let q_id =
    switch (
      Id.Map.fold(
        (id, info, acc) =>
          switch (acc, info) {
          | (None, Info.InfoExp({user_term: {term: Var("q"), _}, _})) =>
            Some(id)
          | _ => acc
          },
        map0,
        None,
      )
    ) {
    | Some(id) => id
    | None => fail("no use of q")
    };
  let probe_ids = Id.Map.singleton(q_id, ());
  let captured = (info_map, elab) => {
    let targets =
      CachedStatics.compute_targets(~settings, ~info_map, ~probe_ids, ());
    let eval_info =
      EvalInfo.of_info_map(~probe_all=false, ~targets, info_map);
    let (_, state) =
      Evaluator.evaluate(~eval_info, ~env=Builtins.env_init, elab);
    Id.Map.cardinal(EvaluatorState.get_probes(state));
  };
  let (map_m, elab_m) =
    Statics.mk_unmemoized(~probe_ids, settings, ctx, term);
  let ds0 = DefStatics.calc(~settings, term);
  let ds = DefStatics.calc(~settings, ~prev=ds0, ~probe_ids, term);
  check(
    bool,
    "probe toggle re-analyzes a strict subset",
    true,
    DefStatics.last_analyzed^ < List.length(ds.items),
  );
  let root = Exp.rep_id(term);
  /* eval reuse keys on probe_targets: a root missing the witness would
     replay without samples */
  let witnessed = (map, id) =>
    switch (Statics.Map.lookup_exp(id, map)) {
    | Some(info) =>
      !SubexpProbeTargets.equal(info.probe_targets, SubexpProbeTargets.empty)
    | None => fail("no info at " ++ Id.show(id))
    };
  List.iter(
    id =>
      check(
        bool,
        "witness as monolithic",
        witnessed(map_m, id),
        witnessed(ds.merged, id),
      ),
    [root, q_id],
  );
  /* guards against the spine patch re-reading its own output */
  let root_co_size = (t: DefStatics.t) =>
    switch (Statics.Map.lookup_exp(root, t.merged)) {
    | Some(info) =>
      CoCtx.to_list(info.co_ctx)
      |> List.fold_left((n, (_, es)) => n + List.length(es), 0)
    | None => (-1)
    };
  let ds_i1 = DefStatics.calc(~settings, ~prev=ds, ~probe_ids, term);
  let ds_i2 = DefStatics.calc(~settings, ~prev=ds_i1, ~probe_ids, term);
  let ds_i3 = DefStatics.calc(~settings, ~prev=ds_i2, ~probe_ids, term);
  check(
    int,
    "spine patch idempotent (co_ctx does not grow)",
    root_co_size(ds_i1),
    root_co_size(ds_i3),
  );
  let ds_off = DefStatics.calc(~settings, ~prev=ds, term);
  check(
    bool,
    "toggle-off clears the root witness",
    false,
    witnessed(ds_off.merged, root),
  );
  switch (DefStatics.whole_elab(ds)) {
  | None => fail("whole_elab: shape gap")
  | Some(elab_c) =>
    check(
      bool,
      "compositional probe capture parity",
      captured(map_m, elab_m) > 0,
      captured(ds.merged, elab_c) > 0,
    )
  };
};

/* at every chunk of a yielding evaluation, the incremental stream
   collector agrees with the full-walk one */
let stream_collector_parity = (): unit => {
  let settings = CoreSettings.on;
  let ctx = Builtins.ctx_init(Some(Operators.default_mode));
  let term = MakeTerm.go(CorpusUtil.mega_seg("mega-1k.hz")).term;
  let (info_map, elab) = Statics.mk_unmemoized(settings, ctx, term);
  let eval_info =
    EvalInfo.of_info_map(
      ~probe_all=false,
      ~targets=Sample.no_targets,
      info_map,
    );
  let merged = ref(IncrEval.empty_outbox);
  let inc = ref(None);
  let chunks = ref(0);
  let compare_states = () => {
    let walk = StreamCollector.collect_stream_state(merged^, elab);
    let (inc', fast) =
      StreamCollector.collect_stream_state_inc(~prev=inc^, merged^, elab);
    inc := inc';
    let differ = (what, get) =>
      compare(get(walk), get(fast)) == 0 ? [] : [what];
    switch (
      differ("probes", EvaluatorState.get_probes)
      @ differ("tests", EvaluatorState.get_tests)
    ) {
    | [] => ()
    | ds =>
      fail(
        "chunk "
        ++ string_of_int(chunks^)
        ++ ": the incremental collector's "
        ++ String.concat(" and ", ds)
        ++ " differ from the walk's",
      )
    };
  };
  let rec drive = ev =>
    switch (Evaluator.run_yielding_slice(~step_budget=2000, ev)) {
    | Evaluator.EvaluationYielded(ev) =>
      let update = Evaluator.drain_streaming_outbox(ev);
      if (!IncrEval.outbox_is_empty(update)) {
        merged := IncrEval.merge_outbox(update, merged^);
        incr(chunks);
        compare_states();
      };
      drive(ev);
    | Evaluator.EvaluationCompleted((_, final_state)) =>
      let (_, fast) =
        StreamCollector.collect_stream_state_inc(~prev=inc^, merged^, elab);
      check(
        bool,
        "final streamed tests <= evaluated tests",
        true,
        List.length(EvaluatorState.get_tests(fast))
        <= List.length(EvaluatorState.get_tests(final_state)),
      );
    };
  drive(
    Evaluator.start_yielding_evaluation(
      ~eval_info,
      ~env=Builtins.env_init,
      elab,
    ),
  );
  check(bool, "chunks streamed", true, chunks^ > 0);
};

let tests = (
  "BenchStatics",
  [
    test_case("stream collector parity", `Quick, stream_collector_parity),
    test_case("probe capture parity", `Quick, probe_capture_parity),
  ],
);
