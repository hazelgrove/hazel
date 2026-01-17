open Alcotest;
open Test_Evaluator_Prelude;

let tests = (
  "Evaluator.List",
  [
    test_case("Ascription goes through cons", `Quick, () => {
      parse_and_evaluate_test(
        "? : [Int]",
        {|let x :: y = ((1 :: ?): [Int]) in y|},
      )
    }),
    test_case("Inconsistent list ascription", `Quick, () =>
      parse_and_evaluate_test(
        "[1 : String,2 : String,3 : String]",
        {|[1,2,3] : [String]|},
      )
    ),
    test_case("List ascription through indet cons", `Quick, () => {
      parse_and_evaluate_test(
        "(x : Int) :: (y : [Int])",
        {|(x :: y) : [Int]|},
      )
    }),
    test_case("Ascription through indet list concatenation", `Quick, () => {
      parse_and_evaluate_test(
        "(x : [Int]) @ (y : [Int])",
        {|(x @ y) : [Int]|},
      )
    }),
    test_case("Inconsistent list ascription with alias", `Quick, () => {
      parse_and_evaluate_test(
        "[1 : String,2 : String,3 : String]",
        {|type T = [String] in [1,2,3] : T|},
      )
    }),
    test_case("List cons passes type inside", `Quick, () => {
      parse_and_evaluate_test({|[1, "hello" : Int]|}, {|1 :: ["hello"]|})
    }),
  ],
);
