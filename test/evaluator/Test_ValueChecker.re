/* Whether an evaluated program is a value: a finished form, not a stuck
   result. The evaluator leaves a stuck result wrapped in a closure, at any
   depth, and a value bare or as a function in a closure. */
open Alcotest;
open Language;
open Test_Evaluator_Prelude;

let check_is_value = (expected, src) =>
  check(
    bool,
    src,
    expected,
    ValueChecker.is_value(parse_and_evaluate(src)),
  );

let tests = (
  "ValueChecker",
  [
    test_case("A finished form is a value", `Quick, () =>
      List.iter(
        [
          "1",
          "fun x -> x",
          "typfun a -> 1",
          "let f = fun x -> f(x) in f",
          "let x = fun y -> x in x",
          "[fun x -> x, fun y -> y]",
          "(1, [2])",
        ],
        ~f=check_is_value(true),
      )
    ),
    test_case("A stuck result is not a value", `Quick, () =>
      List.iter(
        [
          "if ? then 1 else 2",
          "let y = 1 in if ? then y else 2",
          "[if ? then 1 else 2]",
          "(1, (if ? then 1 else 2))",
          "[[if ? then 1 else 2]]",
          "(1, case ? | 1 => 2 end)",
        ],
        ~f=check_is_value(false),
      )
    ),
  ],
);
