open Alcotest;
open Haz3lcore;
open Language;

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
  /* chunks the incremental collector left to the full walk: a parity
     check alone would pass if it always did */
  let fallbacks = ref(0);
  let compare_states = () => {
    let walk = StreamCollector.collect_stream_state(merged^, elab);
    let (inc', fast) =
      StreamCollector.collect_stream_state_inc(~prev=inc^, merged^, elab);
    if (inc' == None) {
      incr(fallbacks);
    };
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
  Printf.printf("STREAMINC fallbacks=%d of %d chunks\n", fallbacks^, chunks^);
  check(int, "incremental collector never falls back", 0, fallbacks^);
};

/* IncrEval.copy_descendant_entries stops at function bodies, where no
   entry is ever recorded: on a real cache it copies exactly what the full
   walk (kept here as the reference) copies */
let copy_descendants_parity = (): unit => {
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
  let (_, state) =
    Evaluator.evaluate(~eval_info, ~env=Builtins.env_init, elab);
  let prev = state.incr_eval;
  let reference = (~root_id, ~root: Exp.t) => {
    let acc = ref([]);
    let _ =
      TermBase.Exp.map_term(
        ~f_exp=
          (continue, e: Exp.t) => {
            let id = Exp.rep_id(e);
            if (!Id.equal(id, root_id) && Id.Map.mem(id, prev.entries)) {
              acc := [id, ...acc^];
            };
            continue(e);
          },
        root,
      );
    List.sort_uniq(Id.compare, acc^);
  };
  let roots = Id.Map.bindings(prev.entries);
  let with_descendants = ref(0);
  let mismatches = ref(0);
  List.iter(
    ((root_id, entry: IncrEval.entry(EvaluatorState.t))) => {
      let want = reference(~root_id, ~root=entry.prev_elab);
      let got =
        IncrEval.copy_descendant_entries(
          ~root_id,
          ~root=entry.prev_elab,
          ~prev,
          IncrEval.empty,
        ).
          entries
        |> Id.Map.bindings
        |> List.map(fst)
        |> List.sort_uniq(Id.compare);
      if (want != []) {
        incr(with_descendants);
      };
      if (!List.equal(Id.equal, want, got)) {
        incr(mismatches);
      };
    },
    roots,
  );
  Printf.printf(
    "COPYDESC entries=%d with_descendants=%d mismatches=%d\n",
    List.length(roots),
    with_descendants^,
    mismatches^,
  );
  check(bool, "some entries have descendants", true, with_descendants^ > 10);
  check(int, "same entries as the full walk", 0, mismatches^);
};

let tests = (
  "StreamCollector",
  [
    test_case(
      "incremental = full walk on mega-1k",
      `Quick,
      stream_collector_parity,
    ),
    test_case(
      "copy_descendant_entries = full walk on mega-1k",
      `Quick,
      copy_descendants_parity,
    ),
  ],
);
