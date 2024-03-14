open Junit_alcotest;
open Test_TypeAssignment;

let (suite, _) =
  run_and_report(
    ~and_exit=false,
    "Dynamics",
    [("Elaboration", Test_Elaboration.elaboration_tests)],
  );
Junit.to_file(Junit.make([suite]), "junit_tests.xml");
Bisect.Runtime.write_coverage_data();

let l = QCheck.Gen.generate(~n=5, Test_TypeAssignment.tree_gen);
List.iter(
  tr => {
    print_endline("\nTree=\n");
    print_endline(Tree.show(tr));
  },
  l,
);
