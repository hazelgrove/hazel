open Alcotest;
open Language;
open Test_Evaluator_Prelude;

/* ========== PatternMatch unit tests ========== */

let pat_match_tests = {
  open IdTagged.FreshGrammar;

  let equal_match_result =
      (r1: PatternMatch.match_result, r2: PatternMatch.match_result)
      : bool =>
    switch (r1, r2) {
    | (DoesNotMatch, DoesNotMatch) => true
    | (IndetMatch, IndetMatch) => true
    | (Matches(env1), Matches(env2)) =>
      List.equal(
        Environment.equal_binding(Language.Exp.fast_equal),
        List.sort(compare, env1),
        List.sort(compare, env2),
      )
    | _ => false
    };

  let check_match = (msg, expected, pat, exp) =>
    check(
      testable(PatternMatch.pp_match_result, equal_match_result),
      msg,
      expected,
      PatternMatch.matches(Util.Id.Map.empty, pat, exp).matches,
    );

  (
    "AddPattern.PatternMatch",
    [
      /* Both-const: 1 + 2 matched against 3 */
      test_case("Both const, match", `Quick, () =>
        check_match(
          "1 + 2 matches 3",
          Matches([]),
          Pat.(add(Operators.Int, int(1), int(2))),
          Exp.(int(3)),
        )
      ),
      /* Both-const: 1 + 2 matched against 4 => does not match */
      test_case("Both const, no match", `Quick, () =>
        check_match(
          "1 + 2 does not match 4",
          DoesNotMatch,
          Pat.(add(Operators.Int, int(1), int(2))),
          Exp.(int(4)),
        )
      ),
      /* Right-const: x + 1 matched against 5 => x = 4 */
      test_case("Right const, var left", `Quick, () =>
        check_match(
          "x + 1 = 5 gives x = 4",
          Matches([("x", Exp.(int(4)))]),
          Pat.(add(Operators.Int, var("x"), int(1))),
          Exp.(int(5)),
        )
      ),
      /* Left-const: 1 + x matched against 5 => x = 4 */
      test_case("Left const, var right", `Quick, () =>
        check_match(
          "1 + x = 5 gives x = 4",
          Matches([("x", Exp.(int(4)))]),
          Pat.(add(Operators.Int, int(1), var("x"))),
          Exp.(int(5)),
        )
      ),
      /* Neither-const: x + y matched against 5 => IndetMatch */
      test_case("Neither const, indet", `Quick, () =>
        check_match(
          "x + y = 5 is indeterminate",
          IndetMatch,
          Pat.(add(Operators.Int, var("x"), var("y"))),
          Exp.(int(5)),
        )
      ),
      /* Nested Add: (1 + 2) + x matched against 6 => x = 3 */
      test_case("Nested Add, left const sum", `Quick, () =>
        check_match(
          "(1 + 2) + x = 6 gives x = 3",
          Matches([("x", Exp.(int(3)))]),
          Pat.(add(Operators.Int, add(Operators.Int, int(1), int(2)), var("x"))),
          Exp.(int(6)),
        )
      ),
    ],
  );
};

/* ========== End-to-end parse+elaborate+evaluate tests ========== */

let eval_tests = (
  "AddPattern.Eval",
  [
    /* Basic: right-const subtraction */
    test_case("let x + 1 = 5 in x", `Quick, () =>
      parse_and_evaluate_test("4", {|let x + 1 = 5 in x|})
    ),
    /* Basic: left-const subtraction */
    test_case("let 1 + x = 5 in x", `Quick, () =>
      parse_and_evaluate_test("4", {|let 1 + x = 5 in x|})
    ),
    /* Both const, matching */
    test_case("let 2 + 3 = 5 in true", `Quick, () =>
      parse_and_evaluate_test("true", {|let 2 + 3 = 5 in true|})
    ),
    /* Nested constant folding */
    test_case("let (1 + 2) + x = 10 in x", `Quick, () =>
      parse_and_evaluate_test("7", {|let (1 + 2) + x = 10 in x|})
    ),
    /* With type annotation: Int */
    test_case("let (x + 1) : Int = 5 in x", `Quick, () =>
      parse_and_evaluate_test("4", {|let (x + 1) : Int = 5 in x|})
    ),
    /* Float mode via annotation */
    test_case("let (x + 1.0) : Float = 5.0 in x", `Quick, () =>
      parse_and_evaluate_test("4.000000", {|let (x + 1.0) : Float = 5.0 in x|})
    ),
    /* Nat mode via annotation */
    test_case("let (x + 1) : Nat = 5 in x", `Quick, () => {
      let result = parse_and_evaluate({|let (x + 1) : Nat = 5 in x|});
      check(
        testable_exp(),
        "nat subtraction result",
        IdTagged.FreshGrammar.Exp.(nat(Bigint.of_int(4))),
        result,
      );
    }),
    /* Case expression with Add pattern */
    test_case("case with add pattern", `Quick, () =>
      parse_and_evaluate_test(
        "4",
        {|case 5
          | x + 1 => x
        end|},
      )
    ),
    /* Multiple bindings in Add */
    test_case("let x + 1 = 10 in x + 3", `Quick, () =>
      parse_and_evaluate_test("12", {|let x + 1 = 10 in x + 3|})
    ),
  ],
);

/* ========== Coverage / exhaustiveness tests ========== */

let coverage_tests = (
  "AddPattern.Coverage",
  [
    /* Int: x + 1 is exhaustive (any int n = (n-1) + 1) */
    Test_Coverage.no_errors(
      "Int: [x + 1] exhaustive",
      {|
let n : Int = ? in
case n
  | x + 1 => x
end|},
    ),
    /* Int: both-const add pattern with wildcard is exhaustive */
    Test_Coverage.no_errors(
      "Int: [1 + 2, _] exhaustive",
      {|
let n : Int = ? in
case n
  | 1 + 2 => true
  | _ => false
end|},
    ),
    /* Int: both-const add without wildcard is non-exhaustive */
    Test_Coverage.has_errors(
      "Int: [1 + 2] non-exhaustive",
      {|
let n : Int = ? in
{{{case n
  | 1 + 2 => true
end}}}|},
      [
        Info.Exp(
          InexhaustiveMatch(
            None,
            Grammar.Pat(
              IdTagged.FreshGrammar.Pat.(big_int(Bigint.of_int(0))),
            ),
          ),
        ),
      ],
    ),
    /* TODO: Nat coverage needs constraint system extension.
       Currently [x + 1] is treated as exhaustive for Nat,
       but it should NOT be — 0 can't be written as x + 1 for any Nat x.
       These tests document the current (wrong) behavior. */
    /* Nat: [0, x + 1] should be exhaustive */
    Test_Coverage.no_errors(
      "Nat: [0, x + 1] exhaustive",
      {|
let n : Nat = ? in
case n
  | 0 => 0
  | x + 1 => x
end|},
    ),
    /* Nat: [0, 1, x + 2] should be exhaustive */
    Test_Coverage.no_errors(
      "Nat: [0, 1, x + 2] exhaustive",
      {|
let n : Nat = ? in
case n
  | 0 => 0
  | 1 => 1
  | x + 2 => x
end|},
    ),
  ],
);

let tests = [pat_match_tests, eval_tests, coverage_tests];
