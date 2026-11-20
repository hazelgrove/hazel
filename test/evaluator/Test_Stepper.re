open Alcotest;
open Language;
open Test_Evaluator_Prelude;

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
    /* Regression: `{ let y = 1; let y = y }` lowers to
       `let y = 1 in let y = y in …`; small-step substitution must not
       capture the inner definition's `y` (found by the confluence
       property test). */
    test_case(
      "Stepper: module item rebinds same name",
      `Quick,
      () => {
        let prog = {|{ let y = 1; let y = y }|};
        let elaborated = elaborate(parse_exp(prog));
        let bigstep =
          Evaluator.evaluate_and_limit(
            ~env=Builtins.env_init,
            ~step_limit=1000,
            elaborated,
          );
        let smallstep =
          full_small_step_reduction(~step_limit=1000, elaborated);
        switch (bigstep, smallstep) {
        | (
            LimitedCompleted((bigstep_exp, _)),
            LimitedCompleted((smallstep_exp, _)),
          ) =>
          check(
            dhexp_typ,
            "small-step result matches big-step result",
            bigstep_exp,
            smallstep_exp,
          )
        | (StepLimitExceeded, _)
        | (_, StepLimitExceeded) =>
          Alcotest.fail("evaluation hit step limit")
        };
      },
    ),
    /* The same program without the module wrapper. */
    test_case(
      "Stepper: nested let rebinds same name (no module)",
      `Quick,
      () => {
        let prog = {|let y = 1 in let y = y in y|};
        let elaborated = elaborate(parse_exp(prog));
        let bigstep =
          Evaluator.evaluate_and_limit(
            ~env=Builtins.env_init,
            ~step_limit=1000,
            elaborated,
          );
        let smallstep =
          full_small_step_reduction(~step_limit=1000, elaborated);
        switch (bigstep, smallstep) {
        | (
            LimitedCompleted((bigstep_exp, _)),
            LimitedCompleted((smallstep_exp, _)),
          ) =>
          check(
            dhexp_typ,
            "small-step result matches big-step result",
            bigstep_exp,
            smallstep_exp,
          )
        | (StepLimitExceeded, _)
        | (_, StepLimitExceeded) =>
          Alcotest.fail("evaluation hit step limit")
        };
      },
    ),
  ],
);
