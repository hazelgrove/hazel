open Junit_alcotest;
open Haz3lcore;
open Test_TypeAssignment;

let (suite, _) =
  run_and_report(
    ~and_exit=false,
    "Dynamics",
    [("Elaboration", Test_Elaboration.elaboration_tests)],
  );
Junit.to_file(Junit.make([suite]), "junit_tests.xml");
Bisect.Runtime.write_coverage_data();

//let l = QCheck.Gen.generate(~n=5, Test_TypeAssignment.uexp_bool_gen);
let u = uexp_gen([], utyp(Int), 2);
List.iter(
  u => {
    print_endline("\nUExp=\n");
    print_endline(Term.UExp.show(u));
  },
  [u],
);
