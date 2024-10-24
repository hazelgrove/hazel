open Junit_alcotest;

let (suite, _) =
  run_and_report(
    ~and_exit=false,
    "Dynamics",
    [
      ("Parser", Test_Menhir.tests),
      ("Statics", Test_Statics.tests),
      ("Evaluator", Test_Evaluator.tests),
    ]
    @ Test_Elaboration.tests,
  );
Junit.to_file(Junit.make([suite]), "junit_tests.xml");
