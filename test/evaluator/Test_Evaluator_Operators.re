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
    test_case(
      "Ascriptions around indet operators collapse",
      `Quick,
      () => {
        parse_and_evaluate_test("x + y", "(x + y) : Int");
        parse_and_evaluate_test("x - y", "(x - y) : Int");
        parse_and_evaluate_test("x * y", "(x * y) : Int");
        parse_and_evaluate_test("x ** y", "(x ** y) : Int");
        parse_and_evaluate_test("x / y", "(x / y) : Int");
        parse_and_evaluate_test("x < y", "(x < y) : Bool");
        parse_and_evaluate_test("x <= y", "(x <= y) : Bool");
        parse_and_evaluate_test("x > y", "(x > y) : Bool");
        parse_and_evaluate_test("x >= y", "(x >= y) : Bool");
        parse_and_evaluate_test(
          ~ignore_dynamic_errors=true,
          "x == y",
          "(x == y) : Bool",
        );
        parse_and_evaluate_test(
          ~ignore_dynamic_errors=true,
          "x != y",
          "(x != y) : Bool",
        );
        parse_and_evaluate_test("s1 ++ s2", "(s1 ++ s2) : String");
        parse_and_evaluate_test("s1 +. s2", "(s1 +. s2) : Float");
        parse_and_evaluate_test("s1 -. s2", "(s1 -. s2) : Float");
        parse_and_evaluate_test("s1 *. s2", "(s1 *. s2) : Float");
        parse_and_evaluate_test("s1 **. s2", "(s1 **. s2) : Float");
        parse_and_evaluate_test("s1 /. s2", "(s1 /. s2) : Float");
        parse_and_evaluate_test("b1 && b2", "(b1 && b2) : Bool");
        parse_and_evaluate_test("b1 || b2", "(b1 || b2) : Bool");
      },
    ),
  ],
);
