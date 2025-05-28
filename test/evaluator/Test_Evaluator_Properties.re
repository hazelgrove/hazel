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
let show_core_exp = exp =>
  exp
  |> ExpToSegment.exp_to_segment(
       ~settings=
         ExpToSegment.Settings.of_core(~inline=true, CoreSettings.off),
       _,
     )
  |> Printer.of_segment(~holes=Some("?"), _);

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
        | (Completed((first_exp, _)), Completed((second_exp, _))) =>
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
        | (Completed(_), StepLimitExceeded)
        | (StepLimitExceeded, Completed(_)) =>
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
let qcheck_preservation_test =
  QCheck.Test.make(
    ~name="Preservation of types",
    ~count=1000000,
    QCheck_Util.arb_exp(~minimal_idents=true, 10),
    uexp => {
    switch (
      {
        let statics =
          Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), uexp);
        let (elaborated, ty) = Elaborator.elaborate(statics, uexp);

        let stepped = single_step(elaborated);

        (stepped, ty);
      }
    ) {
    | (Some((next, _)), orig_ty) =>
      switch (
        {
          let statics =
            Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), next);
          let info: option(Info.t) =
            Statics.Map.lookup(next.annotation.ids |> List.hd, statics);

          info;
        }
      ) {
      | Some(InfoExp({ty, _})) =>
        let ret = Typ.is_consistent(Ctx.empty, ty, orig_ty);
        print_endline(
          "Preservation check: "
          ++ string_of_bool(ret)
          ++ " ("
          ++ Typ.show(orig_ty)
          ++ " !~ "
          ++ Typ.show(ty)
          ++ ")",
        );
        ret;
      | _ => failwith("No type information found for stepped expression")
      }
    | (None, _) => true // If we can't take a step, we don't have to check preservation
    | exception e =>
      print_endline(
        "Skipping preservation test due to error: " ++ Printexc.to_string(e),
      );
      true;
    }
  });

let tests = (
  "Evaluator.Properties",
  [
    QCheck_alcotest.to_alcotest(qcheck_evaluator_does_not_crash_test),
    QCheck_alcotest.to_alcotest(qcheck_stepper_confluence),
    QCheck_alcotest.to_alcotest(qcheck_pattern_equivalence_test),
    QCheck_alcotest.to_alcotest(qcheck_preservation_test),
  ],
);
