open Alcotest;
open Test_Evaluator_Prelude;

/* Tests for stuck tuple destructuring (Feature B):
 * `let <irrefutable-tuple-pat> = SCRUT in body` where SCRUT is indet
 * relative to the pattern's tuple shape steps to
 * `let <pat> = (SCRUT.0, SCRUT.1, ...) in body` and continues. */

let tests = (
  "Evaluator.StuckLet",
  [
    /* === Regression: existing irrefutable-let behavior preserved === */
    test_case("Concrete tuple unchanged", `Quick, () =>
      parse_and_evaluate_test("3", "let (a, b) = (1, 2) in a + b")
    ),
    test_case("Mixed concrete/indet element unchanged", `Quick, () =>
      parse_and_evaluate_test("? + 2", "let (a, b) = (?, 2) in a + b")
    ),
    test_case("Triple concrete", `Quick, () =>
      parse_and_evaluate_test("6", "let (a, b, c) = (1, 2, 3) in a + b + c")
    ),
    test_case("Nested concrete", `Quick, () =>
      parse_and_evaluate_test(
        "6",
        "let ((a, b), c) = ((1, 2), 3) in a + b + c",
      )
    ),
    test_case("Single var pattern unchanged", `Quick, () =>
      parse_and_evaluate_test("?", "let x = ? in x")
    ),
    /* === Refutable patterns must NOT destructure: stay stuck === */
    test_case("Cons pattern stays stuck", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        "let Cons(h, t) = ? in h",
        "let Cons(h, t) = ? in h",
      )
    ),
    test_case("Infix-cons pattern stays stuck", `Quick, () =>
      parse_and_evaluate_test("let h :: t = ? in h", "let h :: t = ? in h")
    ),
    test_case("ListLit pattern stays stuck", `Quick, () =>
      parse_and_evaluate_test("let [a, b] = ? in a", "let [a, b] = ? in a")
    ),
    test_case("Mixed tuple-with-cons stays stuck", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        "let (a, Cons(h, t)) = ? in a",
        "let (a, Cons(h, t)) = ? in a",
      )
    ),
    /* === Stuck destructure: hole scrutinee === */
    test_case("Pair pattern, hole scrut, no body refs", `Quick, () =>
      parse_and_evaluate_test("42", "let (a, b) = ? in 42")
    ),
    test_case("Pair pattern, hole scrut, ref to first", `Quick, () =>
      parse_and_evaluate_test("?.0", "let (a, b) = ? in a")
    ),
    test_case("Pair pattern, hole scrut, ref to second", `Quick, () =>
      parse_and_evaluate_test("?.1", "let (a, b) = ? in b")
    ),
    test_case("Pair pattern, hole scrut, both used", `Quick, () =>
      parse_and_evaluate_test("?.0 + ?.1", "let (a, b) = ? in a + b")
    ),
    test_case("Pair pattern, scrut+arith on body collapses", `Quick, () =>
      parse_and_evaluate_test("2 + ?.0", "let (a, b) = ? in 1 + 1 + a")
    ),
    test_case("Triple pattern - middle access", `Quick, () =>
      parse_and_evaluate_test("?.1", "let (a, b, c) = ? in b")
    ),
    /* === Nested === */
    test_case("Nested pair-in-pair pattern", `Quick, () =>
      parse_and_evaluate_test("(?.0).0", "let ((a, b), c) = ? in a")
    ),
    test_case("Nested mixed access", `Quick, () =>
      parse_and_evaluate_test(
        "(?.0).0 + (?.0).1 + ?.1",
        "let ((a, b), c) = ? in a + b + c",
      )
    ),
    test_case("Nested - access only outermost", `Quick, () =>
      parse_and_evaluate_test("?.1", "let ((a, b), c) = ? in c")
    ),
    /* === Labeled tuple destructure — projection still positional === */
    test_case("Labeled destructure - first", `Quick, () =>
      parse_and_evaluate_test("?.0", "let (a=a', b=b') = ? in a'")
    ),
    test_case("Labeled destructure - second", `Quick, () =>
      parse_and_evaluate_test("?.1", "let (a=a', b=b') = ? in b'")
    ),
    test_case("Labeled destructure - both", `Quick, () =>
      parse_and_evaluate_test("?.0 + ?.1", "let (a=a', b=b') = ? in a' + b'")
    ),
    /* === Type-ascribed pattern: ascription is preserved through match === */
    test_case(
      "Asc pattern descends, ascription preserved on bound var", `Quick, () =>
      parse_and_evaluate_test("?.0 : Int", "let (a: Int, b) = ? in a")
    ),
    test_case("Asc pattern: unascribed slot is plain projection", `Quick, () =>
      parse_and_evaluate_test("?.1", "let (a: Int, b) = ? in b")
    ),
    /* === Wild slot leaves the projection unused but harmless === */
    test_case("Wild in first slot", `Quick, () =>
      parse_and_evaluate_test("?.1", "let (_, b) = ? in b")
    ),
    test_case("Wild in second slot", `Quick, () =>
      parse_and_evaluate_test("?.0", "let (a, _) = ? in a")
    ),
    /* === Indet (non-tuple) scrutinee from a function call ===
       A scrutinee that doesn't reduce to a literal tuple triggers
       stuck-destructure (a literal tuple already matches normally). */
    test_case("Stuck destructure: var bound to hole", `Quick, () =>
      parse_and_evaluate_test(
        "?.0 + ?.1",
        "let p = ? in let (a, b) = p in a + b",
      )
    ),
    test_case("Stuck destructure: function returning hole", `Quick, () =>
      parse_and_evaluate_test(
        "?.0 + ?.1",
        "let f = fun x -> ? in let (a, b) = f(0) in a + b",
      )
    ),
    test_case("Stuck destructure: identity-like fun on hole", `Quick, () =>
      parse_and_evaluate_test(
        "?.0",
        "let id = fun x -> x in let (a, b) = id(?) in a",
      )
    ),
    /* === Refutable: definitively-failing matches stay stuck (no destructure) === */
    test_case("Cons on empty list stays stuck", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        "let Cons(h, t) = [] in h",
        "let Cons(h, t) = [] in h",
      )
    ),
    /* === FunAp with tuple parameter: same rewrite applies on argument === */
    test_case("FunAp tuple-pattern with hole arg destructures", `Quick, () =>
      parse_and_evaluate_test("?.0 + ?.1", "(fun (a, b) -> a + b)(?)")
    ),
    test_case("FunAp tuple-pattern, body uses only one var", `Quick, () =>
      parse_and_evaluate_test("?.0", "(fun (a, b) -> a)(?)")
    ),
    test_case("FunAp nested tuple-pattern", `Quick, () =>
      parse_and_evaluate_test("(?.0).0", "(fun ((a, b), c) -> a)(?)")
    ),
    test_case("FunAp tuple-pattern, concrete arg unchanged", `Quick, () =>
      parse_and_evaluate_test("3", "(fun (a, b) -> a + b)((1, 2))")
    ),
    test_case("FunAp refutable pattern stays stuck", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        "(fun Cons(h, t) -> h)(?)",
        "(fun Cons(h, t) -> h)(?)",
      )
    ),
    /* === Empty-tuple pattern stays stuck (no bound vars to destructure) === */
    test_case("Empty tuple pattern stays stuck (no bindings)", `Quick, () =>
      parse_and_evaluate_test("let () = ? in 1", "let () = ? in 1")
    ),
    test_case("All-wild tuple pattern stays stuck (no bindings)", `Quick, () =>
      parse_and_evaluate_test("let (_, _) = ? in 1", "let (_, _) = ? in 1")
    ),
    /* === Concrete non-tuple scrutinee: destructure fires anyway,
         resulting projections are stuck Dots of a wrong-type value.
         This is the feature's point: the body evaluates; stuckness is
         localized to the specific projection references.

         Caveat: a stuck `Dot(Atom(Int 1), Atom(Int 0))` prints as `1.0`
         which visually collides with the float literal `1.0`. In these
         tests, the expected string uses `(1).0` (parens-wrapped) so it
         parses unambiguously as a Dot. The `1.0`-vs-`1.0` printer
         ambiguity is a display concern, not a dynamics one. */
    test_case(
      "Int scrutinee destructures: body `a` → stuck projection of 1",
      `Quick,
      () =>
      parse_and_evaluate_test("(1).0", "let (a, b) = 1 in a")
    ),
    test_case(
      "Int scrutinee, body uses a.0: chained stuck projection",
      `Quick,
      () =>
      /* a ↦ Dot(Int 1, Int 0); body `a.0` becomes Dot(Dot(1,0), 0). */
      parse_and_evaluate_test(
        "((1).0).0",
        "let (a, b) = 1 in a.0",
      )
    ),
    test_case("Wrong-arity tuple scrutinee still destructures", `Quick, () =>
      /* (1, 2, 3) with pattern (a, b): matches IndetMatch on arity.
         Rewrite produces ((1,2,3).0, (1,2,3).1); first projection
         succeeds at Dot → 1. */
      parse_and_evaluate_test(
        "1",
        "let (a, b) = (1, 2, 3) in a",
      )
    ),
    test_case(
      "Function scrutinee stays stuck (Fun becomes Closure at req_final)",
      `Quick,
      () =>
      /* A fun value is wrapped in Closure by wrap_closure before we see
         it; the Closure-exclusion in is_destructurable_scrut keeps the
         Let stuck. See docs/positional-dot-and-stuck-destructure.md for
         the Closure-exclusion rationale (probe duplication). */
      parse_and_evaluate_test(
        "let (a, b) = fun x -> x in a",
        "let (a, b) = fun x -> x in a",
      )
    ),
  ],
);
