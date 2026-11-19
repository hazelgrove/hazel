open Alcotest;
open Test_Evaluator_Prelude;

/* Tests for positional tuple access (Feature A: x.0, x.1, ...).
 *
 * Companion: Test_Evaluator_StuckLet.re for Feature B (let-pattern
 * destructure rewrite that builds on positional dot). */

let tests = (
  "Evaluator.TupleIndex",
  [
    /* === Basic positional access === */
    test_case("First element of triple", `Quick, () =>
      parse_and_evaluate_test("1", "(1, 2, 3).0")
    ),
    test_case("Middle element of triple", `Quick, () =>
      parse_and_evaluate_test("2", "(1, 2, 3).1")
    ),
    test_case("Last element of triple", `Quick, () =>
      parse_and_evaluate_test("3", "(1, 2, 3).2")
    ),
    test_case("Pair first element", `Quick, () =>
      parse_and_evaluate_test("1", "(1, 2).0")
    ),
    test_case("Pair second element", `Quick, () =>
      parse_and_evaluate_test("2", "(1, 2).1")
    ),
    /* === Different element types === */
    test_case("Mixed types - first", `Quick, () =>
      parse_and_evaluate_test("1", {|(1, "a", true).0|})
    ),
    test_case("Mixed types - middle", `Quick, () =>
      parse_and_evaluate_test({|"a"|}, {|(1, "a", true).1|})
    ),
    test_case("Mixed types - last", `Quick, () =>
      parse_and_evaluate_test("true", {|(1, "a", true).2|})
    ),
    /* === Nested tuples === */
    test_case("Nested - outer access", `Quick, () =>
      parse_and_evaluate_test("(1, 2)", "((1, 2), 3).0")
    ),
    test_case(
      "Nested - chained access (space-separated to avoid float lex)",
      `Quick,
      () =>
      parse_and_evaluate_test("2", "((1, 2), 3) . 0 . 1")
    ),
    test_case("Nested - chained access (outer parens)", `Quick, () =>
      parse_and_evaluate_test("2", "(((1, 2), 3).0).1")
    ),
    test_case("Nested - chained other branch", `Quick, () =>
      parse_and_evaluate_test("3", "((1, 2), 3).1")
    ),
    test_case("Triple nested (outer parens to disambiguate)", `Quick, () =>
      parse_and_evaluate_test("42", "((((42, 0), 0), 0).0 . 0).0")
    ),
    /* === Chained positional access without workarounds === */
    test_case("Chained access x.0.1 parses as Dot(Dot)", `Quick, () =>
      parse_and_evaluate_test("2", "((1, 2), 3).0.1")
    ),
    test_case("Chained access triple", `Quick, () =>
      parse_and_evaluate_test("42", "(((42, 0), 0), 0).0.0.0")
    ),
    test_case("Chained access through labels", `Quick, () =>
      parse_and_evaluate_test("2", "((a=1, b=2), 3).0.1")
    ),
    test_case("Chained access - nested, both chained", `Quick, () =>
      parse_and_evaluate_test("5", "((1, 2), (3, 4, 5), 6).1.2")
    ),
    /* Float literals typed directly must still work (not disrupted by the
       chained-dot guard). */
    test_case("Float literal still works: 0.5", `Quick, () =>
      parse_and_evaluate_test("0.5", "0.5")
    ),
    test_case("Float in arithmetic: 1.0 +. 0.5", `Quick, () =>
      parse_and_evaluate_test("1.5", "1.0 +. 0.5")
    ),
    /* === Indeterminate scrutinees: stuck access stays as syntax === */
    test_case("Stuck on hole scrutinee", `Quick, () =>
      parse_and_evaluate_test("?.0", "?.0")
    ),
    test_case("Stuck on hole - nested", `Quick, () =>
      parse_and_evaluate_test("?.0.1", "?.0.1")
    ),
    test_case("Hole element extracted", `Quick, () =>
      parse_and_evaluate_test("?", "(?, 2).0")
    ),
    test_case("Non-hole element next to hole", `Quick, () =>
      parse_and_evaluate_test("2", "(?, 2).1")
    ),
    /* === Labeled tuples allow positional access too === */
    test_case("Labeled tuple positional 0", `Quick, () =>
      parse_and_evaluate_test("1", "(a=1, b=2).0")
    ),
    test_case("Labeled tuple positional 1", `Quick, () =>
      parse_and_evaluate_test("2", "(a=1, b=2).1")
    ),
    test_case("Labeled tuple still allows label access", `Quick, () =>
      parse_and_evaluate_test("1", "(a=1, b=2).a")
    ),
    test_case("Mixed labeled / positional in same tuple", `Quick, () =>
      parse_and_evaluate_test("99", "(a=99, 2, c=3).0")
    ),
    /* === Through let binding === */
    test_case("Bound var positional access", `Quick, () =>
      parse_and_evaluate_test("1", "let x = (1, 2, 3) in x.0")
    ),
    test_case("Bound var with type ascription", `Quick, () =>
      parse_and_evaluate_test("1", "let x : (Int, Bool) = (1, true) in x.0")
    ),
    test_case("Bound var ascribed - bool slot", `Quick, () =>
      parse_and_evaluate_test(
        "true",
        "let x : (Int, Bool) = (1, true) in x.1",
      )
    ),
    /* === Arithmetic and composition === */
    test_case("Arithmetic on positional access", `Quick, () =>
      parse_and_evaluate_test("3", "(1, 2).0 + (1, 2).1")
    ),
    test_case("Composition - access then add", `Quick, () =>
      parse_and_evaluate_test("10", "let p = (3, 7) in p.0 + p.1")
    ),
    /* === List of tuples — mirror labeled behavior of mapping === */
    test_case("List of tuples - positional map", `Quick, () =>
      parse_and_evaluate_test("[1, 3]", "[(1, 2), (3, 4)].0")
    ),
    test_case("List of tuples - positional map second", `Quick, () =>
      parse_and_evaluate_test("[2, 4]", "[(1, 2), (3, 4)].1")
    ),
    /* === Out of bounds: dynamics stays indet (statics emits a mark) === */
    test_case("Out of bounds stays indet", `Quick, () =>
      parse_and_evaluate_test("(1, 2).5", "(1, 2).5")
    ),
    test_case("Out of bounds in expression context", `Quick, () =>
      parse_and_evaluate_test("(1, 2).5 + 1", "(1, 2).5 + 1")
    ),
    /* === Space-separated index form (for clarity or when chaining) === */
    test_case("Space-separated index", `Quick, () =>
      parse_and_evaluate_test("1", "(1, 2, 3) . 0")
    ),
  ],
);
