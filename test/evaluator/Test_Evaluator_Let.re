open Alcotest;
open Test_Evaluator_Prelude;
let tests = (
  "Evaluator.Let",
  [
    test_case("Inconsisent type ascription on let", `Quick, () =>
      parse_and_evaluate_test("(4 : String)", {|let x : String = 4  in x|})
    ),
  ],
);
