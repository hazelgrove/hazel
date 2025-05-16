open Alcotest;
open Haz3lcore;
open Test_Evaluator_Prelude;

open IdTagged.FreshGrammar;
open Exp;

let tests = (
  "Evaluator.Atom",
  [
    test_case("Integer literal", `Quick, () =>
      evaluation_test("8", int(8), int(8))
    ),
    test_case("Inconsistent type ascription", `Quick, () =>
      parse_and_evaluate_test("(4 : String)", {|(4 : String)|})
    ),
    test_case("Consistent type ascription", `Quick, () =>
      parse_and_evaluate_test("4", {|4 : Int|})
    ),
  ],
);
