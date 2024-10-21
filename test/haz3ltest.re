open Junit_alcotest;

let (suite, _) =
  run_and_report(
    ~and_exit=false,
    "Dynamics",
    [
      ("Statics", Test_Statics.tests),
      // ("MakeTerm", Test_MakeTerm.tests),
      ("Elaboration", Test_Elaboration.elaboration_tests),
      // ("LabeledTuple", Test_LabeledTuple.tests),
      // ("Evaluator", Test_Evaluator.tests),
    ],
  );
Junit.to_file(Junit.make([suite]), "junit_tests.xml");
