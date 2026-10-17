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
  "StreamCollector",
  [
    test_case(
      "incremental = full walk on mega-1k",
      `Quick,
      stream_collector_parity,
    ),
  ],
);
