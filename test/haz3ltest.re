open Junit_alcotest;

let (suite, _) =
  run_and_report(
    ~and_exit=false,
    "HazelTests",
    [
      ("Elaboration", Test_Elaboration.elaboration_tests),
      Test_ListUtil.tests,
      Test_Printer.tests,
    ],
  );
Junit.to_file(Junit.make([suite]), "junit_tests.xml");
