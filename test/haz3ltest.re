open Junit_alcotest;

let (suite, _) =
  run_and_report(
    ~and_exit=false,
    "HazelTests",
    [
      ("MakeTerm", Test_MakeTerm.tests),
      ("LabeledTuple", Test_LabeledTuple.tests),
      ("Statics", Test_Statics.tests),
      ("Elaboration", Test_Elaboration.elaboration_tests),
      ("Evaluator", Test_Evaluator.tests),
      Test_ListUtil.tests,
    ],
  );
Junit.to_file(Junit.make([suite]), "junit_tests.xml");
Bisect.Runtime.write_coverage_data();
