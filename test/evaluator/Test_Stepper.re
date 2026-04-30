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
          step_limited(dhexp_typ),
          "1. +. 2. = 3.",
          Completed(float(3.)),
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
          step_limited(dhexp_typ),
          "(fun x -> x + 1)(5)",
          Completed(int(6)),
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
    /* Regression: a module value that re-binds the same name and
       references the previously-bound value:

         { let y = 1; let y = y }

       Both items use a `let` pattern, so `ExpandModule.expand`
       produces the nested form

         let y = 1 in let y = y in (y = y)

       and the labeled-tuple body should evaluate to `(y = 1)`.

       Surfaced by the `Evaluator.Properties` confluence
       property test, which produced (after one shrink step):

           Expected: (y=y')      -- small-step
           Received: (y=1)       -- big-step

       The small-step substitution failed to substitute the inner
       `y` reference. The fact that this only manifests inside a
       Module's nested-let expansion (not in a hand-written
       `let y = 1 in let y = y in y`) suggests something specific
       to how the module expansion's outer `Let` interacts with
       the stepper's substitution. */
    test_case("Stepper: module item rebinds same name", `Quick, () => {
      let prog = {|{ let y = 1; let y = y }|};
      let elaborated = elaborate(parse_exp(prog));
      let bigstep =
        Evaluator.evaluate_and_limit(
          ~env=Builtins.env_init,
          ~step_limit=100,
          elaborated,
        );
      let smallstep = full_small_step_reduction(~step_limit=100, elaborated);
      switch (bigstep, smallstep) {
      | (Completed((bigstep_exp, _)), Completed(smallstep_exp)) =>
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
    }),
    /* Sanity check: the same-shape program *without* the module
       wrapper. If this passes while the wrapped version fails, the
       bug is specific to how the stepper handles
       `ExpandModule.expand`'s nested-let output. If both fail, the
       stepper has a more general shadowing-substitution issue. */
    test_case("Stepper: nested let rebinds same name (no module)", `Quick, () => {
      let prog = {|let y = 1 in let y = y in y|};
      let elaborated = elaborate(parse_exp(prog));
      let bigstep =
        Evaluator.evaluate_and_limit(
          ~env=Builtins.env_init,
          ~step_limit=100,
          elaborated,
        );
      let smallstep = full_small_step_reduction(~step_limit=100, elaborated);
      switch (bigstep, smallstep) {
      | (Completed((bigstep_exp, _)), Completed(smallstep_exp)) =>
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
    }),
  ],
);
