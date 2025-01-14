open Junit_alcotest;

let (suite, _) =
  run_and_report(
    ~and_exit=false,
    "HazelTests",
    [
      Test_Menhir.tests,
      Test_StringUtil.tests,
      Test_Statics.tests,
      Test_Evaluator.tests,
      Test_ListUtil.tests,
      Test_MakeTerm.tests,
      Test_ExpToSegment.tests,
    ]
    @ Test_Elaboration.tests,
  );
Junit.to_file(Junit.make([suite]), "junit_tests.xml");
Bisect.Runtime.write_coverage_data();
