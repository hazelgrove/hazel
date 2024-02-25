open Alcotest;

run(
  "Dynamics",
  [
    ("Elaboration", Test_Elaboration.elaboration_tests),
    ("Parser", Test_Menhir.tests) // TODO This should probably be separated at the library level and not under dynamics
  ],
);
