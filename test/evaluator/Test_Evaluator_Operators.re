open Alcotest;
open Test_Evaluator_Prelude;
open Language.IdTagged.FreshGrammar;
open Exp;

let tests = (
  "Evaluator.Operators",
  [
    test_case("Integer sum", `Quick, () =>
      evaluation_test("4 + 5", int(9), bin_op(Int(Plus), int(4), int(5)))
    ),
    test_case("Negative integer literal", `Quick, () =>
      evaluation_test("-8", int(-8), un_op(Int(Minus), int(8)))
    ),
    test_case("Inconsistent type ascription in subterm", `Quick, () =>
      parse_and_evaluate_test("1 + (4 : String)", {|1 + (4 : String)|})
    ),
  ],
);
