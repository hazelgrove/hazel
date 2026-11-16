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

let step_redex = (expression, step) =>
  switch (Exp.find_by_id(EvaluatorStep.get_step_id(step), expression)) {
  | Some(redex) => redex
  | None =>
    Alcotest.fail("offered step has no matching expression in the program")
  };

/* Record the actual user-visible redexes, including unexpected step kinds. */
let visible_trace = (~limit, initial) => {
  let rec run = (remaining, exp, visible) =>
    if (remaining <= 0) {
      Alcotest.fail("filtered trace did not finish within the step limit");
    } else {
      let take = (step, visible) =>
        switch (EvaluatorStep.take_step(step)) {
        | None => Alcotest.fail("offered step could not be taken")
        | Some(next) => run(remaining - 1, next, visible)
        };
      switch (step_status(exp)) {
      | AutoStep(step) => take(step, visible)
      | AvailableSteps([]) => (List.rev(visible), exp)
      | AvailableSteps([step, ..._]) =>
        take(
          step,
          [DHExp.strip_ascriptions(step_redex(exp, step)), ...visible],
        )
      };
    };
  run(limit, initial, []);
};

let filter_trace_test = (name, source, expected_steps, expected_result) =>
  test_case(
    name,
    `Quick,
    () => {
      let (steps, result) =
        visible_trace(~limit=500, parse_exp(source) |> elaborate);
      check(
        list(dhexp_typ),
        "visible redexes",
        List.map(parse_exp, expected_steps),
        steps,
      );
      check(dhexp_typ, "final result", parse_exp(expected_result), result);
    },
  );

let filter_matches = (~denv=Environment.empty, pattern, expression) =>
  FilterMatcher.matches_exp(
    ~denv,
    expression,
    ~fenv=Environment.empty,
    pattern,
  );

let tests = (
  "Evaluator.Stepper",
  [
    filter_trace_test(
      "persistent residue survives the one-shot stop in the paper example",
      "debug eval(1 + 2 + 3 + 4) in debug stop(3 + 3 + 4) in 1 + 2 + 3 + 4",
      ["3 + 3"],
      "10",
    ),
    filter_trace_test(
      "one-shot match still records a matching outer persistent filter",
      "debug eval(1 + 2 + 3) in debug stop(1 + 2 + 3) in 1 + 2 + 3",
      ["1 + 2"],
      "6",
    ),
    filter_trace_test(
      "one-shot hide keeps the outer persistent step action",
      "debug step(1 + 2 + 3) in debug hide(1 + 2 + 3) in 1 + 2 + 3",
      ["3 + 3"],
      "6",
    ),
    filter_trace_test(
      "inner persistent filter overrides an outer one-shot filter",
      "debug stop($e) in debug eval($e) in 1 + 2 + 3",
      [],
      "6",
    ),
    filter_trace_test(
      "filter completion is silent even under an explicit step filter",
      "debug step($e) in debug eval($e) in 7",
      [],
      "7",
    ),
    test_case(
      "lower or equal residue levels preserve the enclosing action and context",
      `Quick,
      () => {
      IdTagged.FreshGrammar.Exp.(
        List.iter(
          inner_level => {
            let redex = bin_op(Int(Plus), int(1), int(2));
            let expected = residue(~lvl=2, ~act=(Step, All), int(3));
            let expression =
              residue(
                ~lvl=2,
                ~act=(Step, All),
                residue(~lvl=inner_level, ~act=(Eval, All), redex),
              );
            switch (step_status(expression)) {
            | AutoStep(_) =>
              Alcotest.fail(
                "a lower or tied residue replaced the enclosing action",
              )
            | AvailableSteps([step]) =>
              check(
                dhexp_typ,
                "selected redex",
                redex,
                step_redex(expression, step),
              );
              check(
                option(dhexp_typ),
                "redundant residue is absent after taking the selected step",
                Some(expected),
                EvaluatorStep.take_step(step),
              );
            | AvailableSteps(_) =>
              Alcotest.fail("expected one visible addition")
            };
          },
          [1, 2],
        )
      )
    }),
    test_case(
      "a lower residue does not overwrite an action selected at an ancestor",
      `Quick,
      () => {
        open IdTagged.FreshGrammar.Exp;
        let redex = bin_op(Int(Plus), int(1), int(2));
        let whole = bin_op(Int(Plus), redex, int(3));
        let expression =
          filter(
            ~pat=redex,
            ~act=(Eval, All),
            filter(
              ~pat=whole,
              ~act=(Step, One),
              bin_op(
                Int(Plus),
                residue(~lvl=1, ~act=(Eval, All), redex),
                int(3),
              ),
            ),
          );
        switch (step_status(expression)) {
        | AvailableSteps([step]) =>
          check(
            dhexp_typ,
            "higher-level stop remains active",
            redex,
            step_redex(expression, step),
          )
        | AutoStep(_) =>
          Alcotest.fail("lower residue replaced the higher action")
        | AvailableSteps(_) => Alcotest.fail("expected one visible addition")
        };
      },
    ),
    test_case(
      "both operands remain independently selectable",
      `Quick,
      () => {
        open IdTagged.FreshGrammar.Exp;
        let left = bin_op(Int(Plus), int(1), int(2));
        let right = bin_op(Int(Plus), int(3), int(4));
        let expression = bin_op(Int(Plus), left, right);
        switch (step_status(expression)) {
        | AvailableSteps(steps) =>
          check(
            Alcotest.int,
            "both candidates are offered",
            2,
            List.length(steps),
          );
          let redexes =
            List.map(step => step_redex(expression, step), steps);
          List.iter(
            expected =>
              check(
                Alcotest.bool,
                "operand is offered before its sibling is reduced",
                true,
                List.exists(
                  actual => Alcotest.equal(dhexp_typ, expected, actual),
                  redexes,
                ),
              ),
            [left, right],
          );
        | AutoStep(_) =>
          Alcotest.fail("unfiltered arithmetic should be visible")
        };
      },
    ),
    test_case(
      "a silent operand is reduced before any visible operand is offered",
      `Quick,
      () => {
        open IdTagged.FreshGrammar.Exp;
        let left = bin_op(Int(Plus), int(1), int(2));
        let right = bin_op(Int(Plus), int(3), int(4));
        let expression =
          filter(
            ~pat=right,
            ~act=(Eval, One),
            bin_op(Int(Plus), left, right),
          );
        switch (step_status(expression)) {
        | AutoStep(step) =>
          check(
            dhexp_typ,
            "silent right operand",
            right,
            step_redex(expression, step),
          )
        | AvailableSteps(_) =>
          Alcotest.fail("visible operand offered before silent work")
        };
      },
    ),
    test_case(
      "$v matches actual functions and closure values",
      `Quick,
      () => {
        open IdTagged.FreshGrammar;
        let wildcard = Exp.filter_selector(Val);
        let function_value = Exp.fn(Pat.var("x"), Exp.var("x"), None, None);
        List.iter(
          expression =>
            check(
              Alcotest.bool,
              "value matches",
              true,
              filter_matches(wildcard, expression),
            ),
          [
            Exp.int(3),
            Exp.bool(true),
            function_value,
            Exp.closure(Environment.empty, function_value),
            Exp.closure(Environment.empty, Exp.int(3)),
          ],
        );
      },
    ),
    test_case(
      "$v rejects variables, holes, and unevaluated expressions",
      `Quick,
      () => {
        open IdTagged.FreshGrammar.Exp;
        let wildcard = filter_selector(Val);
        List.iter(
          expression =>
            check(
              Alcotest.bool,
              "non-value does not match",
              false,
              filter_matches(wildcard, expression),
            ),
          [var("x"), empty_hole(), bin_op(Int(Plus), int(1), int(2))],
        );
      },
    ),
    test_case(
      "$v does not match function-body variables, even with an outer binding",
      `Quick,
      () => {
        open IdTagged.FreshGrammar;
        let pattern =
          Exp.fn(Pat.var("x"), Exp.filter_selector(Val), None, None);
        let expression = Exp.fn(Pat.var("y"), Exp.var("y"), None, None);
        check(
          Alcotest.bool,
          "bound variable rejected",
          false,
          filter_matches(pattern, expression),
        );
        check(
          Alcotest.bool,
          "function binder shadows the runtime environment",
          false,
          filter_matches(
            ~denv=Environment.of_list([("y", Exp.int(3))]),
            pattern,
            expression,
          ),
        );
      },
    ),
    test_case(
      "$v rejects fixpoint-bound variables while FixF remains transparent",
      `Quick,
      () => {
        open IdTagged.FreshGrammar;
        check(
          Alcotest.bool,
          "fixpoint body variable rejected",
          false,
          filter_matches(
            Exp.fix_f(Pat.var("x"), Exp.filter_selector(Val), None),
            Exp.fix_f(Pat.var("y"), Exp.var("y"), None),
          ),
        );
        check(
          Alcotest.bool,
          "structural matching still ignores FixF",
          true,
          filter_matches(
            Exp.int(3),
            Exp.fix_f(Pat.var("loop"), Exp.int(3), None),
          ),
        );
      },
    ),
    test_case(
      "filter matching preserves alpha-equivalence and binder shadowing",
      `Quick,
      () => {
      List.iter(
        ((pattern, expression, expected)) =>
          check(
            Alcotest.bool,
            pattern ++ " against " ++ expression,
            expected,
            filter_matches(parse_exp(pattern), parse_exp(expression)),
          ),
        [
          ("fun x -> x", "fun y -> y", true),
          ("fun x -> fun x -> x", "fun a -> fun b -> b", true),
          ("fun x -> fun y -> x", "fun a -> fun b -> b", false),
        ],
      )
    }),
    test_case(
      "$v accepts fully evaluated partial applications",
      `Quick,
      () => {
        open IdTagged.FreshGrammar;
        let fn =
          Exp.fn(
            Pat.tuple([Pat.var("x"), Pat.var("y")]),
            Exp.var("x"),
            None,
            None,
          );
        let partial =
          Exp.deferred_ap(fn, [Exp.int(3), Exp.deferral(InAp)]);
        check(
          Alcotest.bool,
          "deferred argument is a placeholder, not a hole",
          true,
          filter_matches(Exp.filter_selector(Val), partial),
        );
      },
    ),
    test_case(
      "$v rejects unevaluated partial-application arguments",
      `Quick,
      () => {
        open IdTagged.FreshGrammar;
        let fn =
          Exp.fn(
            Pat.tuple([Pat.var("x"), Pat.var("y")]),
            Exp.var("x"),
            None,
            None,
          );
        List.iter(
          argument =>
            check(
              Alcotest.bool,
              "ordinary arguments must already be values",
              false,
              filter_matches(
                Exp.filter_selector(Val),
                Exp.deferred_ap(fn, [argument, Exp.deferral(InAp)]),
              ),
            ),
          [
            Exp.bin_op(Int(Plus), Exp.int(1), Exp.int(2)),
            Exp.var("x"),
            Exp.empty_hole(),
          ],
        );
      },
    ),
    test_case(
      "$v ignores projectors inside compound values",
      `Quick,
      () => {
        open IdTagged.FreshGrammar.Exp;
        let decorated =
          projector(
            {
              kind: Fold,
              model: "",
            },
            int(3),
          );
        List.iter(
          expression =>
            check(
              Alcotest.bool,
              "projector location does not change value matching",
              true,
              filter_matches(filter_selector(Val), expression),
            ),
          [decorated, tuple([decorated, bool(true)])],
        );
      },
    ),
    test_case("$v rejects tuples containing variables or holes", `Quick, () => {
      IdTagged.FreshGrammar.Exp.(
        List.iter(
          element =>
            check(
              Alcotest.bool,
              "every tuple component must be a value",
              false,
              filter_matches(
                filter_selector(Val),
                tuple([int(3), element]),
              ),
            ),
          [var("x"), empty_hole()],
        )
      )
    }),
    test_case(
      "$v distinguishes constructor values from function applications",
      `Quick,
      () => {
        open IdTagged.FreshGrammar;
        let wildcard = Exp.filter_selector(Val);
        let fn = Exp.fn(Pat.var("x"), Exp.var("x"), None, None);
        let constructed =
          Exp.ap(
            Forward,
            Exp.constructor("Some", Some(Some(Typ.int()))),
            Exp.int(3),
          );
        check(
          Alcotest.bool,
          "constructor application is a value",
          true,
          filter_matches(wildcard, constructed),
        );
        check(
          Alcotest.bool,
          "ordinary function application still needs reduction",
          false,
          filter_matches(wildcard, Exp.ap(Forward, fn, Exp.int(3))),
        );
        check(
          Alcotest.bool,
          "the unapplied function itself is a value",
          true,
          filter_matches(wildcard, fn),
        );
      },
    ),
    test_case(
      "filters use the supplied top-level environment",
      `Quick,
      () => {
        open IdTagged.FreshGrammar.Exp;
        let env = Environment.of_list([("x", int(1))]);
        let pattern = bin_op(Int(Plus), var("x"), int(2));
        let body = bin_op(Int(Plus), int(1), int(2));
        let expression = filter(~pat=pattern, ~act=(Eval, One), body);
        switch (
          EvaluatorStep.get_status(~settings=CoreSettings.on, expression, env)
        ) {
        | AutoStep(step) =>
          check(
            dhexp_typ,
            "the environment-resolved filter hides the addition",
            body,
            step_redex(expression, step),
          );
          switch (
            EvaluatorStep.refresh_step(
              ~settings=CoreSettings.on,
              expression,
              env,
              EvaluatorStep.persist(step),
            )
          ) {
          | Some((Eval, _)) => ()
          | _ =>
            Alcotest.fail("refreshing the step lost its filter environment")
          };
        | AvailableSteps(_) =>
          Alcotest.fail("the matching hide filter was ignored")
        };
      },
    ),
    test_case(
      "filter patterns retain their defining closure environment",
      `Quick,
      () => {
        open IdTagged.FreshGrammar.Exp;
        let outer_env = Environment.of_list([("x", int(1))]);
        let inner_env = Environment.of_list([("x", int(9))]);
        let body = bin_op(Int(Plus), int(1), int(2));
        let expression =
          closure(
            outer_env,
            filter(
              ~pat=bin_op(Int(Plus), var("x"), int(2)),
              ~act=(Eval, One),
              closure(inner_env, body),
            ),
          );
        let normalized = Substitution.in_exp(Environment.empty, expression);
        List.iter(
          program =>
            switch (step_status(program)) {
            | AutoStep(step) =>
              check(
                dhexp_typ,
                "closure representation preserves the hidden redex",
                body,
                step_redex(program, step),
              )
            | AvailableSteps(_) =>
              Alcotest.fail("the defining pattern binding was lost")
            },
          [expression, normalized],
        );
      },
    ),
    test_case(
      "inner closures do not rebind an outer filter pattern",
      `Quick,
      () => {
        open IdTagged.FreshGrammar.Exp;
        let outer_env = Environment.of_list([("x", int(1))]);
        let inner_env = Environment.of_list([("x", int(9))]);
        let body = bin_op(Int(Plus), int(9), int(2));
        let expression =
          closure(
            outer_env,
            filter(
              ~pat=bin_op(Int(Plus), var("x"), int(2)),
              ~act=(Eval, One),
              closure(inner_env, body),
            ),
          );
        switch (step_status(expression)) {
        | AvailableSteps(steps) =>
          check(
            list(dhexp_typ),
            "the outer pattern is 1 + 2, so 9 + 2 stays visible",
            [body],
            List.map(step => step_redex(expression, step), steps),
          )
        | AutoStep(_) =>
          Alcotest.fail("the inner closure rebound the outer filter's x")
        };
      },
    ),
    test_case(
      "target binders shadow the matching environment against literals",
      `Quick,
      () => {
        open IdTagged.FreshGrammar;
        let denv = Environment.of_list([("y", Exp.int(3))]);
        let pattern = Exp.fn(Pat.var("x"), Exp.int(3), None, None);
        check(
          Alcotest.bool,
          "the bound target variable is not the outer value 3",
          false,
          filter_matches(
            ~denv,
            pattern,
            Exp.fn(Pat.var("y"), Exp.var("y"), None, None),
          ),
        );
        check(
          Alcotest.bool,
          "a free target variable still uses its environment",
          true,
          filter_matches(
            ~denv,
            pattern,
            Exp.fn(Pat.var("z"), Exp.var("y"), None, None),
          ),
        );
      },
    ),
    test_case(
      "pattern binders shadow the matching environment against literals",
      `Quick,
      () => {
        open IdTagged.FreshGrammar;
        let fenv = Environment.of_list([("x", Exp.int(3))]);
        let denv = Environment.of_list([("y", Exp.int(9))]);
        let expression = Exp.fn(Pat.var("y"), Exp.int(3), None, None);
        let bound_pattern = Exp.fn(Pat.var("x"), Exp.var("x"), None, None);
        check(
          Alcotest.bool,
          "the bound pattern variable is not the outer value 3",
          false,
          FilterMatcher.matches_exp(~denv, expression, ~fenv, bound_pattern),
        );
        check(
          Alcotest.bool,
          "a free pattern variable still uses its environment",
          true,
          FilterMatcher.matches_exp(
            ~denv,
            expression,
            ~fenv,
            Exp.fn(Pat.var("z"), Exp.var("x"), None, None),
          ),
        );
        check(
          Alcotest.bool,
          "alpha-equivalent binders ignore differing outer bindings",
          true,
          FilterMatcher.matches_exp(
            ~denv,
            Exp.fn(Pat.var("y"), Exp.var("y"), None, None),
            ~fenv,
            bound_pattern,
          ),
        );
      },
    ),
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
