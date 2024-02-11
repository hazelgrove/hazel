open Alcotest;

run(
  ~argv=[|"json"|],
  "Dynamics",
  [("Elaboration", Test_Elaboration.elaboration_tests)],
);
