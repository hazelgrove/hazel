open Junit_alcotest;

let (suite, _) =
  run_and_report(
    ~and_exit=false,
    "Dynamics",
    [
      ("Elaboration", Test_Elaboration.elaboration_tests),
      ("Parser", Test_Menhir.tests),
    ],
  );
Junit.to_file(Junit.make([suite]), "junit_tests.xml");
Bisect.Runtime.write_coverage_data();
