open Alcotest;
open Language;
open Test_Evaluator_Prelude;

let step_status = exp =>
  EvaluatorStep.get_status(~settings=CoreSettings.on, exp, Environment.empty);

let rec steps_until_available = (~limit, exp) =>
  if (limit <= 0) {
    Alcotest.fail("expected available steps before step limit");
  } else {
    switch (step_status(exp)) {
    | AutoStep(step) =>
      switch (EvaluatorStep.take_step(step)) {
      | None => Alcotest.fail("expected auto step")
      | Some(exp') => steps_until_available(~limit=limit - 1, exp')
      }
    | AvailableSteps(steps) => steps
    };
  };

let rec count_available_steps = (~limit, exp, count) =>
  if (limit <= 0) {
    Alcotest.fail("step count exceeded limit");
  } else {
    switch (step_status(exp)) {
    | AutoStep(step) =>
      switch (EvaluatorStep.take_step(step)) {
      | None => count
      | Some(exp') => count_available_steps(~limit=limit - 1, exp', count)
      }
    | AvailableSteps(steps) =>
      switch (steps) {
      | [] => count
      | [step, ..._] =>
        switch (EvaluatorStep.take_step(step)) {
        | None => count
        | Some(exp') =>
          count_available_steps(~limit=limit - 1, exp', count + 1)
        }
      }
    };
  };

let tests = (
  "Evaluator.Stepper",
  [
    test_case(
      "Eval filter auto-steps",
      `Quick,
      () => {
        let exp =
          parse_exp("debug eval($e) in (1 + 2) + (3 + 4)") |> elaborate;
        switch (step_status(exp)) {
        | AutoStep(step) =>
          switch (EvaluatorStep.take_step(step)) {
          | None => Alcotest.fail("expected auto step")
          | Some(exp') =>
            switch (step_status(exp')) {
            | AutoStep(_) => ()
            | AvailableSteps(_) =>
              Alcotest.fail("expected auto step to continue")
            }
          }
        | AvailableSteps(_) => Alcotest.fail("expected AutoStep")
        };
      },
    ),
    test_case(
      "Hide filter only auto-steps once",
      `Quick,
      () => {
        let exp =
          parse_exp("debug hide(1 + 2) in (1 + 2) + (3 + 4)") |> elaborate;
        /* Auto-step through all hidden steps (RemoveParens, filter match, etc.)
           until we get AvailableSteps. The hide filter with (Eval, One) should
           cause stepping to stop after one filter-matched step. */
        let steps = steps_until_available(~limit=20, exp);
        check(bool, "expected visible steps", true, steps != []);
      },
    ),
    test_case(
      "Hide filter does not match non-values",
      `Quick,
      () => {
        let exp = parse_exp("debug hide($v) in 1 + 2") |> elaborate;
        /* $v should not match the non-value expression 1 + 2, so after
           auto-stepping through any hidden steps, we should get visible steps */
        let steps = steps_until_available(~limit=20, exp);
        check(bool, "expected visible steps", true, steps != []);
      },
    ),
    test_case(
      "Stop filter yields visible steps",
      `Quick,
      () => {
        let exp =
          parse_exp("debug stop($v + $v) in (1 + 2) + (3 + 4)") |> elaborate;
        /* stop = (Step, One): after auto-stepping through hidden steps
           (RemoveParens etc.), we should get visible steps */
        let steps = steps_until_available(~limit=20, exp);
        check(bool, "expected visible steps", true, steps != []);
      },
    ),
    test_case(
      "Step filter yields visible steps",
      `Quick,
      () => {
        let exp =
          parse_exp("debug step($v + $v) in (1 + 2) + (3 + 4)") |> elaborate;
        /* step = (Step, All): after auto-stepping through hidden steps,
           we should get visible steps */
        let steps = steps_until_available(~limit=20, exp);
        check(bool, "expected visible steps", true, steps != []);
      },
    ),
    test_case(
      "Unresolved filter wrapper does not corrupt inner filter matching",
      `Quick,
      () => {
        /* An unresolved filter (condition is not a recognized action
           application) is a no-op for stepping. Inserting one between a
           stop filter and its target must not change which steps are
           visible: matching below the Unresolved frame has to keep
           matching against the actual redex, not against the unresolved
           condition expression. */
        let base = "debug eval($e) in debug stop(1 + 2) in (1 + 2) + (3 + 4)";
        let wrapped = "debug eval($e) in debug stop(1 + 2) in debug 42 in (1 + 2) + (3 + 4)";
        let base_steps =
          count_available_steps(~limit=500, parse_exp(base) |> elaborate, 0);
        let wrapped_steps =
          count_available_steps(
            ~limit=500,
            parse_exp(wrapped) |> elaborate,
            0,
          );
        check(
          bool,
          "control program has visible steps",
          true,
          base_steps > 0,
        );
        check(
          int,
          "no-op unresolved filter must not change visible step count",
          base_steps,
          wrapped_steps,
        );
      },
    ),
    test_case(
      "later stop(..) overrides earlier hide(..)",
      `Quick,
      () => {
        let program = {|
debug hide($e) in
let map =
  fun xs, f ->
    case xs
      | [] => []
      | hd :: tl => f(hd) :: map(tl, f)
    end
in
let square = fun x -> x * x in
debug stop(square($v)) in
map([1, 2, 3], square)|};
        let exp = parse_exp(program) |> elaborate;
        let steps = steps_until_available(~limit=200, exp);
        check(bool, "expected visible steps", true, steps != []);
      },
    ),
    test_case(
      "Stop filter map requires multiple steps",
      `Quick,
      () => {
        let program = {|
debug hide($e) in
let map =
  fun xs, f ->
    case xs
      | [] => []
      | hd :: tl => f(hd) :: map(tl, f)
    end
in
let square = fun x -> x * x in
debug stop(square($v)) in
map([1, 2, 3], square)|};
        let exp = parse_exp(program) |> elaborate;
        let steps = count_available_steps(~limit=500, exp, 0);
        check(int, "expected exact 3 steps", 3, steps);
      },
    ),
    test_case(
      "Stop on 1+2 with repeated subterms: persist+refresh roundtrip",
      `Quick,
      () => {
        let program = {|
debug eval($e) in
debug stop(1 + 2) in
1 + 2 + 3 + (1 + 2 + 3 + (1 + 2 + 3))|};
        let exp = parse_exp(program) |> elaborate;
        let rec loop = (n, exp) =>
          if (n <= 0) {
            ();
          } else {
            switch (step_status(exp)) {
            | AutoStep(step) =>
              switch (EvaluatorStep.take_step(step)) {
              | None => ()
              | Some(exp') => loop(n - 1, exp')
              }
            | AvailableSteps(steps) =>
              List.iter(
                (step: EvaluatorStep.step) => {
                  let persistent = EvaluatorStep.persist(step);
                  switch (
                    EvaluatorStep.refresh_step(
                      ~settings=CoreSettings.on,
                      exp,
                      Environment.empty,
                      persistent,
                    )
                  ) {
                  | Some(_) => ()
                  | None =>
                    Alcotest.fail("refresh_step returned None after persist")
                  };
                  ();
                },
                steps,
              );
              switch (steps) {
              | [] => ()
              | [step, ..._] =>
                switch (EvaluatorStep.take_step(step)) {
                | None => ()
                | Some(exp') => loop(n - 1, exp')
                }
              };
            };
          };
        loop(500, exp);
        check(bool, "no failure", true, true);
      },
    ),
    test_case(
      "Stop filter on fac overrides settings-based pre-filter (regression)",
      `Quick,
      () => {
        let program = {|
debug hide($e) in
let fac : Int -> Int =
  fun n ->
    if n < 2 then 1 else n * fac(n - 1)
in
debug stop(fac($v)) in
fac(3)|};
        let exp = parse_exp(program) |> elaborate;
        /* CoreSettings.on has show_fixpoints=false, so FixUnwrap steps are
           pre-filtered out of the default trace. The user-written
           debug stop(fac($v)) must still produce a visible pause at each
           fac(v) redex (fac(3), fac(2), fac(1) -- the recursive base case
           returns directly). Before user filters were allowed to override
           settings-based pre-filtering in should_hide_eval_obj, the count
           was 0 because the filter never got a chance to see the FixUnwrap
           redexes. */
        let steps = count_available_steps(~limit=500, exp, 0);
        check(int, "expected 3 visible fac calls", 3, steps);
      },
    ),
    test_case(
      "No user filter: settings still hide FixUnwrap (no regression)",
      `Quick,
      () => {
        let program = {|
let fac : Int -> Int =
  fun n ->
    if n < 2 then 1 else n * fac(n - 1)
in
fac(3)|};
        let exp = parse_exp(program) |> elaborate;
        /* With no user filter, CoreSettings.on still silences FixUnwrap and
           other pre-filtered step kinds. Evaluation should reach a final
           value via auto-stepping; the few visible pauses (if any) come from
           non-pre-filtered kinds (e.g. user-visible arithmetic). The key
           guarantee: the fac unrolls do NOT become visible just because we
           reordered the matches/settings check. */
        let steps = count_available_steps(~limit=2000, exp, 0);
        check(
          bool,
          "evaluation terminates without runaway visible steps",
          true,
          steps < 50,
        );
      },
    ),
    test_case(
      "step filter does NOT surface unrenderable Asc steps (regression)",
      `Quick,
      () => {
        /* When `step($v + $v)` matches an arithmetic expression that's
           wrapped in ascriptions, the Asc transitions inside should not
           surface as visible (clickable) steps under default settings
           (show_ascriptions=false). Otherwise the stepper UI shows zero
           green boxes for them — they have no surface piece to draw on.
           The user's `step` filter should still pause at the arithmetic
           itself, but auto-take the surrounding Asc transitions. */
        let program = {|
debug step($v + $v) in
let f : Int -> Int = fun x -> x + x in
f(3)|};
        let exp = parse_exp(program) |> elaborate;
        let steps = steps_until_available(~limit=200, exp);
        let kinds =
          List.map(
            s =>
              Transition.stepper_justification(
                EvaluatorStep.get_step_kind(s),
              ),
            steps,
          );
        let has_asc = List.exists(k => k == "ascription transition", kinds);
        check(
          bool,
          "no unrenderable Asc step should be exposed as AvailableStep",
          false,
          has_asc,
        );
      },
    ),
    test_case(
      "Stop filter on fac: persist then refresh_step roundtrip",
      `Quick,
      () => {
        let program = {|
debug eval($e) in
let fac : Int -> Int =
  fun n ->
    if n < 2 then 1 else n * fac(n - 1)
in
debug stop(fac($v)) in
fac(3)|};
        let exp = parse_exp(program) |> elaborate;
        let rec loop = (n, exp) =>
          if (n <= 0) {
            ();
          } else {
            switch (step_status(exp)) {
            | AutoStep(step) =>
              switch (EvaluatorStep.take_step(step)) {
              | None => ()
              | Some(exp') => loop(n - 1, exp')
              }
            | AvailableSteps(steps) =>
              List.iter(
                (step: EvaluatorStep.step) => {
                  let persistent = EvaluatorStep.persist(step);
                  switch (
                    EvaluatorStep.refresh_step(
                      ~settings=CoreSettings.on,
                      exp,
                      Environment.empty,
                      persistent,
                    )
                  ) {
                  | Some(_) => ()
                  | None =>
                    Alcotest.fail("refresh_step returned None after persist")
                  };
                  ();
                },
                steps,
              )
            };
          };
        loop(500, exp);
        check(bool, "no failure", true, true);
      },
    ),
    test_case(
      "Stop filter on fac: persist each step and take manual steps",
      `Quick,
      () => {
        let program = {|
debug eval($e) in
let fac : Int -> Int =
  fun n ->
    if n < 2 then 1 else n * fac(n - 1)
in
debug stop(fac($v)) in
fac(3)|};
        let exp = parse_exp(program) |> elaborate;
        let rec loop = (n, exp) =>
          if (n <= 0) {
            ();
          } else {
            switch (step_status(exp)) {
            | AutoStep(step) =>
              let _ = EvaluatorStep.persist(step);
              switch (EvaluatorStep.take_step(step)) {
              | None => ()
              | Some(exp') => loop(n - 1, exp')
              };
            | AvailableSteps(steps) =>
              List.iter(
                (step: EvaluatorStep.step) => {
                  let _ = EvaluatorStep.persist(step);
                  ();
                },
                steps,
              );
              // Simulate user clicking the first available step.
              switch (steps) {
              | [] => ()
              | [step, ..._] =>
                switch (EvaluatorStep.take_step(step)) {
                | None => ()
                | Some(exp') => loop(n - 1, exp')
                }
              };
            };
          };
        loop(500, exp);
        check(bool, "no failure during manual stepping", true, true);
      },
    ),
    test_case(
      "Simple arithmetic",
      `Quick,
      () => {
        open IdTagged.FreshGrammar.Exp;
        let result =
          full_small_step_reduction(
            bin_op(Float(Plus), float(1.), float(2.)),
          );

        Alcotest.check(
          step_limited,
          "1. +. 2. = 3.",
          LimitedCompleted((float(3.), EvaluatorState.empty)),
          result,
        );
      },
    ),
    test_case(
      "Simple arithmetic with unboxing",
      `Quick,
      () => {
        open IdTagged.FreshGrammar;
        open Exp;
        let result =
          full_small_step_reduction(
            ap(
              Forward,
              fn(
                Pat.var("x"),
                bin_op(Int(Plus), var("x"), int(1)),
                None,
                None,
              ),
              int(5),
            ),
          );

        Alcotest.check(
          step_limited,
          "(fun x -> x + 1)(5)",
          LimitedCompleted((int(6), EvaluatorState.empty)),
          result,
        );
      },
    ),
    test_case("Single step ascription of list", `Quick, () => {
      check(
        option(dhexp_typ),
        "let x  =[1,2,3,4] : [Int] in x -> let x = [1 : Int, 2 : Int, 3 : Int, 4 : Int] in x",
        Some(
          parse_exp({|let x = [1 : Int, 2 : Int, 3 : Int, 4 : Int] in x|}),
        ),
        single_step(
          elaborate(parse_exp({|let x  =[1,2,3,4] : [Int] in x|})),
        ),
      )
    }),
  ],
);
