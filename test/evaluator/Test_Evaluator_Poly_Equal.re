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
          "Equal List 1",
          bool(true),
          elaborate(parse_exp("[1,2,3] != [2,1]")),
        );
        evaluation_test(
          "Equal List 2",
          bool(true),
          elaborate(parse_exp("[true, false] == [true, false]")),
        );
        evaluation_test(
          "Equal Tuples 1",
          bool(true),
          elaborate(parse_exp("(false, \"\") == (false, \"\")")),
        );
        evaluation_test(
          "Equal Tuples 2",
          bool(false),
          elaborate(parse_exp("(false, \"\") == (false, \"str\")")),
        );
        evaluation_test(
          "Equal LabTuples 1",
          bool(true),
          elaborate(
            parse_exp(
              "(\"str\", x= false, y= 1) == (\"str\", x= false, y= 1)",
            ),
          ),
        );
        evaluation_test(
          "Equal LabTuples 2",
          bool(false),
          elaborate(parse_exp("(a= false, b= 1) == (a= false, b= 2)")),
        );
        evaluation_test(
          "TypFun Equality 1",
          bool(true),
          elaborate(
            parse_exp(
              "let f = typfun X -> let x: A+B(X) = A in x in f@<Int> == f@<Int>",
            ),
          ),
        );
        evaluation_test(
          "Equal Type Constructors 1",
          bool(false),
          elaborate(parse_exp("type T = +A+B(Int) in A == B(1)")),
        );
        evaluation_test(
          "Equal Type Constructors 2",
          bool(true),
          elaborate(parse_exp("type T = +A+B(Int) in A == A")),
        );
        evaluation_test(
          "Equal Type Constructors 3",
          bool(false),
          elaborate(parse_exp("type T = +A+B(Int) in B(2) == B(1)")),
        );
        evaluation_test(
          "Indet Variables in sum constructors",
          ~ignore_constructor_types=true,
          parse_exp("C(x) == C(x)"),
          elaborate(parse_exp("type T = +C(Int) in C(x) == C(x)")),
        );
      },
    ),
    test_case(
      "Polymorphic Equality Type Inconsistency Dynamics",
      `Quick,
      () => {
        evaluation_test(
          ~ignore_dynamic_errors=true,
          "Hidden inconsistency caught in dynamics",
          dynamic_error_hole(elaborate(parse_exp("1 == 1.")), Incomparable),
          elaborate(parse_exp("let x: ? = 1 in let y: ? = 1. in x == y")),
        );
        evaluation_test(
          ~ignore_dynamic_errors=true,
          "Hidden containing arrow caught in dynamics",
          dynamic_error_hole(
            elaborate(parse_exp("(fun x -> x) == (fun x -> x)")),
            Incomparable,
          ),
          elaborate(parse_exp("let f = fun x -> x in f == f")),
        );
        evaluation_test(
          ~ignore_dynamic_errors=true,
          "Labeled tuple with different labels",
          dynamic_error_hole(
            elaborate(parse_exp("(a=1, b=2) == (a=1, c=2)")),
            Incomparable,
          ),
          elaborate(parse_exp("(a=1, b=2) == (a=1, c=2)")),
        );
        evaluation_test(
          ~ignore_dynamic_errors=true,
          "Labeled tuple with rearragned labels",
          dynamic_error_hole(
            elaborate(parse_exp("(a=1, b=2) == (b=2, a=1)")),
            Incomparable,
          ),
          elaborate(parse_exp("(a=1, b=2) == (b=2, a=1)")),
        );
        evaluation_test(
          ~ignore_dynamic_errors=true,
          "Typfun is not comparable",
          ~ignore_constructor_types=true,
          dynamic_error_hole(
            elaborate(parse_exp("(typfun X -> 1) == (typfun X -> 1)")),
            Incomparable,
          ),
          elaborate(parse_exp("let f = typfun X -> 1 in f == f")),
        );
        evaluation_test(
          ~ignore_dynamic_errors=true,
          "Hidden containing arrow in tuple caught in dynamics",
          ~ignore_constructor_types=true,
          dynamic_error_hole(
            elaborate(parse_exp("type T2 = +A(Int->Int)+B in B == B")),
            Incomparable,
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
