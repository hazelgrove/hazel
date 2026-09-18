open Alcotest;
open Language;
open Test_Evaluator_Prelude;

let id = testable(Fmt.using(Id.show, Fmt.string), Id.equal);

let module_bind_step = (exp: Exp.t) =>
  switch (
    EvaluatorStep.get_status(
      ~settings=CoreSettings.on,
      exp,
      Environment.empty,
    )
  ) {
  | AutoStep(step) => Some(step)
  | AvailableSteps(steps) =>
    List.find_opt(
      step =>
        switch (EvaluatorStep.get_step_kind(step)) {
        | ModuleBind(_) => true
        | _ => false
        },
      steps,
    )
  };

let tests = (
  "Evaluator.Stepper",
  [
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
    /* Stepping substitutes, where the evaluator closes over an environment:
       a module binding has to reach the items that follow it either way. */
    test_case("Module bindings reach the items that follow", `Quick, () => {
      check(
        step_limited,
        "{ let a = 1; let b = a * 2 } -> { let a = 1; let b = 2 }",
        LimitedCompleted((
          elaborate(parse_exp({|{ let a = 1; let b = 2 }|})),
          EvaluatorState.empty,
        )),
        full_small_step_reduction(
          elaborate(parse_exp({|{ let a = 1; let b = a * 2 }|})),
        ),
      )
    }),
    /* The binding rewrites the whole module, but the step belongs to the item:
       that is what the stepper highlights and what the user clicks. */
    test_case(
      "A module binding step is anchored on its item",
      `Quick,
      () => {
        let exp = elaborate(parse_exp({|{ let a = 1; let b = a * 2 }|}));
        let item_id =
          switch (Exp.term_of(exp)) {
          | Module([item, ..._]) => Mod.rep_id(item)
          | _ => fail("expected a module with items")
          };
        switch (module_bind_step(exp)) {
        | Some(step) =>
          check(
            id,
            "the item, not the module",
            item_id,
            EvaluatorStep.get_step_id(step),
          )
        | None => fail("expected a module binding step")
        };
      },
    ),
  ],
);
