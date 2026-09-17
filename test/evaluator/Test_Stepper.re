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
  ],
);
