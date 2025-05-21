open Junit_alcotest;

Printexc.register_printer(exn => {
  switch (exn) {
  | Haz3lcore.EvaluatorError.Exception(msg) =>
    Some(Haz3lcore.EvaluatorError.show(msg))
  | _ => None
  }
});

let (suite, _) =
  run_and_report(
    ~and_exit=false,
    "HazelTests",
    [
      Test_Grammar.tests,
      Test_ExpToSegment.tests,
      Test_LabeledTuple.tests,
      Test_MakeTerm.tests,
      // Test_Menhir.tests,
      Test_StringUtil.tests,
      Test_Typ.tests,
      (
        "Statics",
        Test_Statics_Functions.tests
        @ Test_Statics_Labeled_Tuple.tests
        @ Test_Statics_Polymorphism.tests
        @ Test_Statics_Sums.tests
        @ Test_Statics_Types.tests
        @ Test_Statics_Property_DoesNotCrash.tests,
      ),
      // @ Test_Elaboration.tests
      // @ Test_Evaluator.tests
      // @ [Test_Coverage.tests, Test_ListUtil.tests, Test_Unboxing.tests]
      // @ Test_Introduce.tests,
    ],
  );
Junit.to_file(Junit.make([suite]), "junit_tests.xml");
Bisect.Runtime.write_coverage_data();
