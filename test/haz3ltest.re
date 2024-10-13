open Junit_alcotest;

let (suite, _) =
  run_and_report(
    ~and_exit=false,
    "Dynamics",
    [
      ("Elaboration", Test_Elaboration.elaboration_tests),
      ("Statics", Test_Statics.tests),
      ("Evaluator", Test_Evaluator.tests),
      ("MakeTerm", Test_MakeTerm.tests),
    ],
  );
Junit.to_file(Junit.make([suite]), "junit_tests.xml");
