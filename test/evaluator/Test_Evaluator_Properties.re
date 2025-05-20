open Haz3lcore;
open Test_Evaluator_Prelude;
open Alcotest;

let qcheck_evaluator_does_not_crash_test =
  QCheck.Test.make(
    ~name="Evaluator does not crash",
    ~count=10000,
    QCheck_Util.arb_exp(~minimal_idents=true, 50),
    exp => {
    switch (
      Elaborator.elaborate(
        Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), exp),
        exp,
      )
      |> fst
    ) {
    | exp =>
      switch (
        Evaluator.evaluate_and_limit(
          ~env=Builtins.env_init,
          ~step_limit=10000,
          exp,
        )
      ) {
      | Completed((_, _))
      | StepLimitExceeded => true
      | exception e =>
        switch (e) {
        | Failure(msg)
            when
              List.exists(
                (==)(msg),
                ["type application in dynamics", "Type join of ap"] // "type application in dynamics" https://github.com/hazelgrove/hazel/issues/1625
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
      Elaborator.elaborate(
        Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), uexp),
        uexp,
      )
      |> fst
    ) {
    | elaborated_exp =>
      switch (
        Evaluator.evaluate_and_limit(
          ~env=Builtins.env_init,
          ~step_limit=100,
          elaborated_exp,
        ),
        full_small_step_reduction(
          ~state=EvaluatorState.init,
          ~step_limit=100,
          elaborated_exp,
        ),
      ) {
      | (Completed((bigstep_exp, _)), Completed(smallstep_exp)) =>
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
          |> Printer.of_segment(~holes=Some("?"), _);

        Alcotest.check(
          testable(Fmt.using(show_core_exp, Fmt.string), DHExp.fast_equal), // Output is easier to view through ExpToSegment. This may result in a loss of information
          "Small step reduction and big step reduction are equal",
          smallstep_exp,
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

let tests = (
  "Evaluator.Properties",
  [
    QCheck_alcotest.to_alcotest(qcheck_evaluator_does_not_crash_test),
    QCheck_alcotest.to_alcotest(qcheck_stepper_confluence),
  ],
);
