open Alcotest;
open Test_Evaluator_Prelude;
open Language;
open IdTagged.FreshGrammar;
open Exp;
let tests = (
  "Evaluator.Poly_Equal",
  [
    test_case(
      "Basic Polymorphic",
      `Quick,
      () => {
        evaluation_test(
          "Equal Integer",
          bool(false),
          elaborate(parse_exp("1 == 2")),
        );
        evaluation_test(
          "Equal Float",
          bool(true),
          elaborate(parse_exp("1. == 1.")),
        );
        evaluation_test(
          "Equal Bool",
          bool(false),
          elaborate(parse_exp("false != false")),
        );
        evaluation_test(
          "Equal String",
          bool(true),
          elaborate(parse_exp("\"str\" == \"str\"")),
        );
        evaluation_test(
          "Equal List",
          bool(true),
          elaborate(parse_exp("[1,2,3] != [2,1]")),
        );
        evaluation_test(
          "Equal Tuples",
          bool(true),
          elaborate(parse_exp("(false, \"\") == (false, \"\")")),
        );
        evaluation_test(
          "Equal Type Constructors",
          bool(false),
          elaborate(parse_exp("type T = +A+B(Int) in A == B(1)")),
        );
        evaluation_test(
          "Equal Type Constructors Same",
          bool(true),
          elaborate(parse_exp("type T = +A+B(Int) in A == A")),
        );
        evaluation_test(
          "Equal Type Constructors Different",
          bool(false),
          elaborate(parse_exp("type T = +A+B(Int) in B(2) == B(1)")),
        );
      },
    ),
    test_case(
      "Polymorphic Equality Type Inconsistency Dynamics",
      `Quick,
      () => {
        evaluation_test(
          "Hidden inconsistency caught in dynamics",
          dynamic_error_hole(elaborate(parse_exp("1 == 1.")), Inconsistent),
          elaborate(parse_exp("let x: ? = 1 in let y: ? = 1. in x == y")),
        );
        evaluation_test(
          "Hidden containing arrow caught in dynamics",
          dynamic_error_hole(
            elaborate(parse_exp("(fun x -> x) == (fun x -> x)")),
            CompareArrow,
          ),
          elaborate(parse_exp("let f = fun x -> x in f == f")),
        );
        evaluation_test(
          "Hidden containing arrow in tuple caught in dynamics",
          dynamic_error_hole(
            elaborate(
              parse_exp(
                "type T2 = +A(Int->Int)+B in let b1 : ? = B in b1 == b1",
              ),
            ),
            CompareArrow,
          ),
          elaborate(
            parse_exp(
              "type T2 = +A(Int->Int)+B in let b1 : ? = B in b1 == b1",
            ),
          ),
        );
      },
    ),
  ],
);
