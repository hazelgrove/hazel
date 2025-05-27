open Alcotest;
open Haz3lcore;
open Test_Evaluator_Prelude;

open IdTagged.FreshGrammar;
open Exp;

let tests = (
  "Evaluator.LabeledTuples",
  [
    test_case(
      "Automatic label insertion in pattern for labeled tuple", `Quick, () =>
      parse_and_evaluate_test(
        "2",
        {|let x : (a=Int) -> Int = fun a -> a in x(2)|},
      )
    ),
    test_case("Cast removal for labels in let pattern", `Quick, () =>{
      parse_and_evaluate_test("(a=1)", {|let x : (a=Int) = (a=1) in x|})}
    ),
    test_case("Labeled tuple field access", `Quick, () =>
      parse_and_evaluate_test("1", {|(a=1,b=2).a|})
    ),
    test_case("Anonymous function with explicit label", `Quick, () => {
      parse_and_evaluate_test(
        "5",
        {|let fn : (a=String) -> Int =
  fun (a=a : String) -> string_length(a)
in fn("hello")|},
      )
    }),
    test_case("Anonymous function without explicit label", `Quick, () => {
      parse_and_evaluate_test(
        "5",
        {|let fn : (a=String) -> Int =
            fun (a : String) -> string_length(a)
          in fn("hello")|},
      )
    }),
    test_case("Inconsistent labels", `Quick, () => {
      parse_and_evaluate_test(
        {|(a=3 : Bool, b= "": Float)|}, // TODO This is a bug
        {|(a=3, b="") : (c=Bool, d=Float)|},
      )
    }),
    test_case("Dot operation for missing label", `Quick, () =>
      parse_and_evaluate_test("(a=1,b=2).c", "(a=1,b=2).c")
    ),
    test_case("Desructuring labeled tuple", `Quick, () =>
      parse_and_evaluate_test(
        "(1, 2, 3.0)",
        {|let (a=a', b=b', c) = (a=1, b=2, 3.0) in (a',b',c)|},
      )
    ),
    test_case("Labeled tuple projection", `Quick, () =>
      evaluation_test(
        "(a=1, b=2, c=?).a",
        int(1),
        dot(
          tuple([
            tup_label(label("a"), int(1)),
            tup_label(label("b"), int(2)),
            tup_label(label("c"), empty_hole()),
          ]),
          label("a"),
        ),
      )
    ),
    test_case("hole field projection", `Quick, () =>
      parse_and_evaluate_test("?", "?.a")
    ), // TODO This should be indet and not a hole
    test_case(
      "Indet projection",
      `Quick,
      () => {
        parse_and_evaluate_test("(true) . a", "(true) . a");
        parse_and_evaluate_test("((true) . a): Int", "((true) . a): Int");
      },
    ),
  ],
);
