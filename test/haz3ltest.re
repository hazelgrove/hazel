open Junit_alcotest;

let (suite, _) =
  run_and_report(
    ~and_exit=false,
    "Dynamics",
    [
      ("Elaboration", Test_Elaboration.elaboration_tests),
      ("Type Assignment", Test_TypeAssignment.type_assignment_tests),
      ("Random tests", Test_TypeAssignment.random_tests),
    ],
  );
Junit.to_file(Junit.make([suite]), "junit_tests.xml");
Bisect.Runtime.write_coverage_data();
