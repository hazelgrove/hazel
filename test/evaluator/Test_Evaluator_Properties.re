open Haz3lcore;
open Language;
open Test_Evaluator_Prelude;
open Alcotest;

let qcheck_evaluator_does_not_crash_test =
  QCheck.Test.make(
    ~name="Evaluator does not crash",
    ~count=10000,
    QCheck_Util.arb_exp(~minimal_idents=true, 50),
    exp => {
    switch (
      {
        let (_, elab) =
          Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), exp);
        elab;
      }
    ) {
    | exp =>
      switch (
        Evaluator.evaluate_and_limit(
          ~env=Builtins.env_init,
          ~step_limit=10000,
          exp,
        )
      ) {
      | LimitedCompleted(_)
      | StepLimitExceeded => true
      | exception e =>
        switch (e) {
        | Failure(msg)
            when
              List.exists(
                (==)(msg),
                ["type application in dynamics", "Type meet of ap"] // "type application in dynamics" https://github.com/hazelgrove/hazel/issues/1625
              ) =>
          print_endline("Skipping failure: " ++ msg);
          true;
        | _ => raise(e)
        }
      }
    | exception e =>
      print_endline(
        "Skipping statics/elaborate failure: " ++ Printexc.to_string(e),
      );
      true;
    }
  });

let qcheck_stepper_confluence =
  QCheck.Test.make(
    ~name="Evaluator and stepper are consistent",
    ~count=1000,
    QCheck_Util.arb_exp(~minimal_idents=true, 10),
    uexp => {
    switch (
      {
        let (_, elab) =
          Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), uexp);
        elab;
      }
    ) {
    | elaborated_exp =>
      switch (
        Evaluator.evaluate_and_limit(
          ~env=Builtins.env_init,
          ~step_limit=100,
          elaborated_exp,
        ),
        full_small_step_reduction(~step_limit=100, elaborated_exp),
      ) {
      | (
          LimitedCompleted((bigstep_exp, _)),
          LimitedCompleted(smallstep_exp),
        ) =>
        let show_core_exp = exp =>
          exp
          |> ExpToSegment.exp_to_segment(
               ~settings=
                 ExpToSegment.Settings.of_core(
                   ~inline=true,
                   CoreSettings.off,
                 ),
               _,
             )
          |> Printer.of_segment(~holes="?", _);

        Alcotest.check(
          testable(
            Fmt.using(show_core_exp, Fmt.string),
            Equality.semantic.exp,
          ), // Output is easier to view through ExpToSegment. This may result in a loss of information
          "Small step reduction and big step reduction are equal",
          smallstep_exp |> fst,
          bigstep_exp,
        );
        true;
      | (_, StepLimitExceeded)
      | (StepLimitExceeded, _) => true
      | exception e =>
        print_endline(
          "Skipping evaluation failure: " ++ Printexc.to_string(e),
        );
        true;
      }
    | exception e =>
      print_endline(
        "Skipping statics/elaborate failure: " ++ Printexc.to_string(e),
      );
      true;
    }
  });
let show_core_exp = exp =>
  exp
  |> ExpToSegment.exp_to_segment(
       ~settings=
         ExpToSegment.Settings.of_core(~inline=true, CoreSettings.off),
       _,
     )
  |> Printer.of_segment(~holes="?", _);

// Property that states let x : T = e in x is equivalent to e : T
let qcheck_pattern_equivalence_test =
  QCheck.Test.make(
    ~name="Pattern equivalence",
    ~count=1000,
    QCheck.pair(
      QCheck_Util.arb_exp(~minimal_idents=true, 40),
      QCheck_Util.arb_typ(~minimal_idents=true, 10),
    ),
    ((uexp, typ)) =>
    try(
      {
        open IdTagged.FreshGrammar;
        open Exp;
        let first = asc(uexp, typ);
        let second = let_(Pat.asc(Pat.var("x"), typ), uexp, var("x"));
        let elaborated_first = elaborate(first);
        let elaborated_second = elaborate(second);

        let evaluated_first =
          Evaluator.evaluate_and_limit(
            ~env=Builtins.env_init,
            ~step_limit=10000,
            elaborated_first,
          );
        let evaluated_second =
          Evaluator.evaluate_and_limit(
            ~env=Builtins.env_init,
            ~step_limit=1000000,
            elaborated_second,
          );
        switch (evaluated_first, evaluated_second) {
        | (
            LimitedCompleted((first_exp, _)),
            LimitedCompleted((second_exp, _)),
          ) =>
          print_endline("First expression: " ++ show_core_exp(first));
          print_endline("Second expression: " ++ show_core_exp(second));
          Alcotest.check(
            dhexp_typ,
            "Evaluated expressions are equal",
            first_exp,
            second_exp,
          );
          true;
        | (StepLimitExceeded, StepLimitExceeded) => true
        | (LimitedCompleted(_), StepLimitExceeded)
        | (StepLimitExceeded, LimitedCompleted(_)) =>
          print_endline("One of the evaluations exceeded the step limit");
          false;
        };
      }
    ) {
    | e =>
      print_endline(
        "Skipping pattern equivalence test due to error: "
        ++ Printexc.to_string(e),
      );
      true;
    }
  );

// Taking a step should result in a consistent type that is more precise than the original type
[@warning "-52"]
let qcheck_preservation_test =
  QCheck.Test.make(
    ~name="Preservation of types",
    ~count=10000,
    QCheck_Util.arb_exp(~minimal_idents=true, 10),
    uexp => {
    switch (
      switch (
        {
          let (statics, elab) =
            Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), uexp);
          let ty = elaborated_type(statics, uexp);
          let stepped = single_step(elab);
          (stepped, ty);
        }
      ) {
      | (Some(next), orig_ty) =>
        switch (
          {
            let (statics, _) =
              Statics.mk(
                CoreSettings.on,
                Builtins.ctx_init(Some(Int)),
                next,
              );
            Statics.Map.ty_of(next.annotation.ids |> List.hd, statics);
          }
        ) {
        | Some(ty) =>
          Typ.is_more_precise(Ctx.empty, ty, orig_ty)
            ? true
            : Alcotest.fail(
                "Preservation failed: original type "
                ++ Typ.show(orig_ty)
                ++ " is not more precise than stepped type "
                ++ Typ.show(ty),
              )
        | _ =>
          Alcotest.fail("No type information found for stepped expression")
        }
      | (None, _) => true // If we can't take a step, we don't have to check preservation
      }
    ) {
    | ret => ret
    | exception (Invalid_argument("List.fold_left2")) // https://github.com/hazelgrove/hazel/issues/1673
    | exception Stack_overflow => true // Known issue with some expressions that cause infinite recursion in the stepper
    }
  });

/* Incremental evaluator correctness:
 *
 *   for all expressions E and edits δ,
 *     eval_incr(δ(E), prev = eval(E))  ==  eval(δ(E))
 *
 * i.e. evaluating the edited expression against the previous run's cache
 * must land on the same value as evaluating it from scratch. Both sides
 * use the SAME elaboration of the edited expression, so any disagreement
 * points squarely at the reuse / dirty-propagation logic rather than at
 * semantics.
 *
 * The edit we apply is a single-literal mutation (flip an Atom(Int n) to
 * Atom(Int n')) with all surrounding ids preserved — this mirrors the
 * Zipper's behaviour on a text edit, where only the touched tokens get
 * new ids. That in-place mutation is what makes the test hit the
 * reuse_check path at all; if we regenerated the tree from source the
 * id spaces would be disjoint and nothing would match prev.
 *
 * Known skips below (return `true`): expressions with no int literal to
 * edit (nothing to test), anything that hits the step limit, and anything
 * that raises from statics/evaluation (filtered the same way as the
 * other evaluator QCheck tests in this file). */

/* Collect every (id, value) pair for Atom(Int _) leaves in the tree.
 * We key on the Atom's own rep_id so the later substitute pass can
 * find and mutate exactly one leaf without touching any sibling ids. */
let collect_int_lits = (exp: Exp.t): list((Id.t, Bigint.t)) => {
  let acc = ref([]);
  let f_exp = (continue, e: Exp.t): Exp.t => {
    switch (e.term) {
    | Atom(Int(n)) => acc := [(Exp.rep_id(e), n), ...acc^]
    | _ => ()
    };
    continue(e);
  };
  let _ = TermBase.Exp.map_term(~f_exp, exp);
  acc^;
};

/* Replace the Atom(Int _) payload at the given id with `to_`, preserving
 * the id on the edited node (and on every other node). */
let replace_int_lit_by_id = (~target: Id.t, ~to_: Bigint.t, exp: Exp.t): Exp.t => {
  let f_exp = (continue, e: Exp.t): Exp.t =>
    if (Id.equal(Exp.rep_id(e), target)) {
      switch (e.term) {
      | Atom(Int(_)) => {
          ...e,
          term: Atom(Int(to_)),
        }
      | _ => continue(e)
      };
    } else {
      continue(e);
    };
  TermBase.Exp.map_term(~f_exp, exp);
};

/* Same wrapper the incremental tests use — statics plus elaboration in
 * one call, so both sides of the equation see identical elab terms. */
let statics_and_elab = (exp: Exp.t): (Statics.Map.t, Exp.t) =>
  Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), exp);

let eval_limited =
    (~prev=IncrEval.empty, ~eval_info, ~step_limit, elab: Exp.t) =>
  Evaluator.evaluate_and_limit(
    ~step_limit,
    ~prev,
    ~eval_info,
    ~env=Builtins.env_init,
    elab,
  );

let qcheck_incremental_matches_fresh_after_edit =
  QCheck.Test.make(
    ~name="Incremental eval agrees with fresh eval after a literal edit",
    ~count=2000,
    QCheck.pair(
      QCheck.small_nat,
      QCheck_Util.arb_exp(~minimal_idents=true, 30),
    ),
    ((seed, exp)) => {
      /* Only swallow known-benign static/dynamic failures so real
       * incremental-eval disagreements surface as clean PBT failures. */
      let try_eval = (~prev=?, eval_info, elab) =>
        try(Some(eval_limited(~prev?, ~eval_info, ~step_limit=10000, elab))) {
        | Failure(msg)
            when
              List.exists(
                (==)(msg),
                ["type application in dynamics", "Type meet of ap"],
              ) =>
          None
        };
      let try_statics = exp =>
        try(Some(statics_and_elab(exp))) {
        | _ => None
        };
      switch (collect_int_lits(exp)) {
      | [] => true /* Nothing to edit — the property is vacuously true. */
      | lits =>
        let (target_id, old_value) =
          List.nth(lits, seed mod List.length(lits));
        /* +1 keeps the type fixed, so the edit typechecks the same as the
         * original while still changing the value at `target_id`. */
        let new_value = Bigint.(old_value + of_int(1));
        let edited =
          replace_int_lit_by_id(~target=target_id, ~to_=new_value, exp);
        switch (try_statics(exp), try_statics(edited)) {
        | (
            Some((info_map_orig, elab_orig)),
            Some((info_map_edit, elab_edit)),
          ) =>
          let info_slice_orig =
            EvalInfo.of_info_map(
              ~probe_all=CoreSettings.on.probe_all,
              ~targets=Id.Map.empty,
              info_map_orig,
            );
          let info_slice_edit =
            EvalInfo.of_info_map(
              ~probe_all=CoreSettings.on.probe_all,
              ~targets=Id.Map.empty,
              info_map_edit,
            );
          /* Baseline run (no prev) of the original — its incr_eval becomes
           * the cache handed to the incremental run of the edited exp. */
          switch (try_eval(info_slice_orig, elab_orig)) {
          | None
          | Some(StepLimitExceeded) => true
          | Some(LimitedCompleted((_, state_before))) =>
            /* Edited evaluated two ways: incrementally (reusing the baseline's
             * cache) and from scratch (empty prev). These must agree. */
            let fresh = try_eval(info_slice_edit, elab_edit);
            let incr_eval_result =
              try_eval(
                ~prev=state_before.incr_eval,
                info_slice_edit,
                elab_edit,
              );
            switch (fresh, incr_eval_result) {
            | (
                Some(LimitedCompleted((e_fresh, _))),
                Some(LimitedCompleted((e_incr, _))),
              ) =>
              Equality.semantic.exp(e_fresh, e_incr)
            | _ => true
            };
          };
        | _ => true
        };
      };
    },
  );

let rec finish_yielding = (~remaining_slices: int, evaluation) => {
  if (remaining_slices <= 0) {
    fail("Yielding evaluation did not complete");
  };
  switch (Evaluator.run_yielding_slice(~step_budget=1, evaluation)) {
  | EvaluationCompleted(value) => value
  | EvaluationYielded(evaluation) =>
    finish_yielding(~remaining_slices=remaining_slices - 1, evaluation)
  };
};

let rec finish_yielding_with_stream =
        (~remaining_slices: int, ~stream, evaluation) => {
  if (remaining_slices <= 0) {
    fail("Yielding evaluation did not complete");
  };
  switch (Evaluator.run_yielding_slice(~step_budget=1, evaluation)) {
  | EvaluationCompleted(value) =>
    let stream =
      IncrEval.add_stream(
        Evaluator.drain_streaming_outbox(evaluation).completed,
        stream,
      );
    (value, stream);
  | EvaluationYielded(evaluation) =>
    let stream =
      IncrEval.add_stream(
        Evaluator.drain_streaming_outbox(evaluation).completed,
        stream,
      );
    finish_yielding_with_stream(
      ~remaining_slices=remaining_slices - 1,
      ~stream,
      evaluation,
    );
  };
};

let yielding_evaluation_test =
  test_case(
    "Yielding evaluation resumes to the synchronous result",
    `Quick,
    () => {
      let (_, exp) =
        Statics.mk(
          CoreSettings.on,
          Builtins.ctx_init(Some(Int)),
          parse_exp("let x = 1 in let y = 2 in x + y"),
        );
      let (sync_exp, _) = Evaluator.evaluate(~env=Builtins.env_init, exp);
      let evaluation =
        Evaluator.start_yielding_evaluation(~env=Builtins.env_init, exp);
      let evaluation =
        switch (Evaluator.run_yielding_slice(~step_budget=1, evaluation)) {
        | EvaluationYielded(evaluation) => evaluation
        | EvaluationCompleted(_) =>
          fail("Expected yielding evaluation to yield with a one-step budget")
        };
      let (yielded_exp, _) =
        finish_yielding(~remaining_slices=1000, evaluation);
      check(dhexp_typ, "yielding evaluation result", sync_exp, yielded_exp);
    },
  );

let yielding_streaming_outbox_test =
  test_case(
    "Yielding evaluation streams completed incremental entries",
    `Quick,
    () => {
      let (info_map, exp) =
        Statics.mk(
          CoreSettings.on,
          Builtins.ctx_init(Some(Int)),
          parse_exp("let x = 1 in let y = x + 2 in y"),
        );
      let eval_info =
        EvalInfo.of_info_map(
          ~probe_all=CoreSettings.on.probe_all,
          ~targets=Id.Map.empty,
          info_map,
        );
      let evaluation =
        Evaluator.start_yielding_evaluation(
          ~eval_info,
          ~env=Builtins.env_init,
          exp,
        );
      let ((_, final_state), stream) =
        finish_yielding_with_stream(
          ~remaining_slices=1000,
          ~stream=IncrEval.empty,
          evaluation,
        );
      check(
        int,
        "streamed entry count matches final entries",
        Id.Map.cardinal(final_state.incr_eval.entries),
        Id.Map.cardinal(stream.entries),
      );
      check(
        bool,
        "every streamed id appears in final entries",
        true,
        Id.Map.for_all(
          (id, _) => Id.Map.mem(id, final_state.incr_eval.entries),
          stream.entries,
        ),
      );
    },
  );

let rec yield_until_current = (~remaining_slices: int, evaluation) => {
  if (remaining_slices <= 0) {
    fail("Yielding evaluation did not produce a current outbox state");
  };
  switch (Evaluator.run_yielding_slice(~step_budget=1, evaluation)) {
  | EvaluationCompleted(_) =>
    fail("Expected yielding evaluation to yield before completion")
  | EvaluationYielded(evaluation) =>
    let outbox = Evaluator.drain_streaming_outbox(evaluation);
    switch (outbox.current) {
    | Some(_) => outbox
    | None =>
      yield_until_current(~remaining_slices=remaining_slices - 1, evaluation)
    };
  };
};

let yielding_streaming_current_state_test =
  test_case(
    "Yielding evaluation streams current partial state",
    `Quick,
    () => {
      let (info_map, exp) =
        Statics.mk(
          CoreSettings.on,
          Builtins.ctx_init(Some(Int)),
          parse_exp("let x = 1 + 2 in let y = x + 3 in y"),
        );
      let eval_info =
        EvalInfo.of_info_map(
          ~probe_all=CoreSettings.on.probe_all,
          ~targets=Id.Map.empty,
          info_map,
        );
      let evaluation =
        Evaluator.start_yielding_evaluation(
          ~eval_info,
          ~env=Builtins.env_init,
          exp,
        );
      let outbox = yield_until_current(~remaining_slices=1000, evaluation);
      switch (outbox.current) {
      | Some({state, _}) =>
        let collected = StreamCollector.collect_stream_state(outbox, exp);
        check(
          bool,
          "current state has dynamic work",
          true,
          state.step_count > 0,
        );
        check(
          bool,
          "collector includes current state",
          true,
          collected.step_count >= state.step_count,
        );
        check(
          bool,
          "current state does not recursively carry incr_eval",
          true,
          Id.Map.is_empty(state.incr_eval.entries),
        );
      | None => fail("Expected current outbox state")
      };
    },
  );

/* Regression: with probes off, stepped terms share Id.invalid (Exp.temp).
 * If outbox.current is keyed by that id, StreamCollector matches it against
 * its own walk temps and truncates — streamed test counts can go backwards
 * mid-run (e.g. 2 -> 1). current must stay keyed by a real program id. */
let rec check_streaming_tests_monotonic =
        (
          ~remaining_slices: int,
          ~prev_test_count: int,
          ~accumulated: IncrEval.outbox(EvaluatorState.t),
          ~eval_info,
          ~exp,
          evaluation,
        ) =>
  if (remaining_slices <= 0) {
    fail("Yielding evaluation did not complete");
  } else {
    switch (Evaluator.run_yielding_slice(~step_budget=1, evaluation)) {
    | EvaluationCompleted(_) =>
      let accumulated =
        IncrEval.merge_outbox(
          Evaluator.drain_streaming_outbox(evaluation),
          accumulated,
        );
      let collected = StreamCollector.collect_stream_state(accumulated, exp);
      TestMap.count(collected.tests);
    | EvaluationYielded(evaluation) =>
      let update = Evaluator.drain_streaming_outbox(evaluation);
      switch (update.current) {
      | Some({id, _}) =>
        check(
          bool,
          "current is never keyed by Id.invalid",
          false,
          Id.equal(id, Id.invalid),
        );
        check(
          bool,
          "current is keyed by a program id",
          true,
          Option.is_some(EvalInfo.find_opt(id, eval_info)),
        );
      | None => ()
      };
      let accumulated = IncrEval.merge_outbox(update, accumulated);
      let collected = StreamCollector.collect_stream_state(accumulated, exp);
      let test_count = TestMap.count(collected.tests);
      check(
        bool,
        "streamed test count is monotonic",
        true,
        test_count >= prev_test_count,
      );
      check_streaming_tests_monotonic(
        ~remaining_slices=remaining_slices - 1,
        ~prev_test_count=test_count,
        ~accumulated,
        ~eval_info,
        ~exp,
        evaluation,
      );
    };
  };

let yielding_streaming_current_id_invalid_race_test =
  test_case(
    "Stream collector current is not keyed by Id.invalid (probes off)",
    `Quick,
    () => {
      let (info_map, exp) =
        Statics.mk(
          CoreSettings.on,
          Builtins.ctx_init(Some(Int)),
          parse_exp(
            "test 1 == 1 end; test 2 == 2 end; test 3 == 3 end; test 4 == 4 end",
          ),
        );
      let eval_info =
        EvalInfo.of_info_map(
          ~probe_all=false,
          ~targets=Id.Map.empty,
          info_map,
        );
      let evaluation =
        Evaluator.start_yielding_evaluation(
          ~eval_info,
          ~env=Builtins.env_init,
          exp,
        );
      let final_count =
        check_streaming_tests_monotonic(
          ~remaining_slices=5000,
          ~prev_test_count=0,
          ~accumulated=IncrEval.empty_outbox,
          ~eval_info,
          ~exp,
          evaluation,
        );
      check(int, "all tests eventually stream", 4, final_count);
    },
  );

let tests = (
  "Evaluator.Properties",
  [
    yielding_evaluation_test,
    yielding_streaming_outbox_test,
    yielding_streaming_current_state_test,
    yielding_streaming_current_id_invalid_race_test,
    QCheck_alcotest.to_alcotest(qcheck_evaluator_does_not_crash_test),
    QCheck_alcotest.to_alcotest(qcheck_stepper_confluence),
    QCheck_alcotest.to_alcotest(qcheck_pattern_equivalence_test),
    QCheck_alcotest.to_alcotest(qcheck_incremental_matches_fresh_after_edit),
    // QCheck_alcotest.to_alcotest(qcheck_preservation_test), // Disabled due to known issues with preservation
  ],
);
