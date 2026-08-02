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
    /* Negation re-kinds by operand/analysis class (replace_un_op_cls):
       `-` on a float is float negation, not an Int type error. */
    test_case("Negative float literal, syn position", `Quick, () =>
      parse_and_evaluate_test({|true|}, {|-1.5 ==. 0. -. 1.5|})
    ),
    test_case("Negation of float variable", `Quick, () =>
      parse_and_evaluate_test({|true|}, {|let x = 2.5 in -x ==. 0. -. 2.5|})
    ),
    test_case("Float analysis retypes negated int literal", `Quick, () =>
      parse_and_evaluate_test(
        {|true|},
        {|let x: Float = -5 in x ==. 0. -. 5.|},
      )
    ),
    test_case("Negative int still Int under negation", `Quick, () =>
      parse_and_evaluate_test({|0|}, {|-5 + 5|})
    ),
    test_case("Negation of parenthesized float expression", `Quick, () =>
      parse_and_evaluate_test({|true|}, {|atan(0. -. 1.) ==. -(atan(1.))|})
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
