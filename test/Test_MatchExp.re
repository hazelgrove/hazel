open Alcotest;

let parse_exp = (s: string) => {
  switch (Haz3lcore.Parser.to_term(s, ~root=Exp)) {
  | Some(e) => e
  | None => Alcotest.fail("Failed to parse expression: " ++ s)
  };
};

let match_check =
    (
      ~info_map=Language.Statics.Map.empty,
      ~alphas=[],
      ~ctx_in=[],
      exp_r,
      exp,
      expected,
      (),
    ) => {
  let exp_r' = parse_exp(exp_r);
  let exp' = parse_exp(exp);
  check(
    testable(
      Fmt.using(
        fun
        | None => "None"
        | Some(x) => Language.MatchExp.show_match_ctx(x),
        Fmt.string,
      ),
      Option.equal(
        List.for_all2(((str1, (typ1, opt1)), (str2, (typ2, opt2))) =>
          str1 == str2
          && Language.Typ.fast_equal(typ1, typ2)
          && Option.equal(Language.Exp.fast_equal, opt1, opt2)
        ),
      ),
    ),
    exp_r ++ " against " ++ exp,
    expected,
    Language.MatchExp.match_exp(
      ~info_map,
      ~alphas,
      ~exp_env=Language.Environment.empty,
      ~exp_r_ctx=ctx_in,
      exp_r',
      exp',
    ),
  );
};

open Language.IdTagged.FreshGrammar;
open Exp;

let hole_typ = Typ.unknown(Internal);

let tests = [
  (
    "MatchExp",
    [
      test_case(
        "Match a variable",
        `Quick,
        match_check(
          ~ctx_in=[("x", (hole_typ, None))],
          "x",
          "y",
          Some([("x", (hole_typ, Some(var("y"))))]),
        ),
      ),
      test_case(
        "Doesn't resolve if different",
        `Quick,
        match_check(
          ~ctx_in=[("x", (hole_typ, None))],
          "(x, x)",
          "(y, z)",
          None,
        ),
      ),
      test_case(
        "Does resolve if same",
        `Quick,
        match_check(
          ~ctx_in=[("x", (hole_typ, None))],
          "(x, x)",
          "(y, y)",
          Some([("x", (hole_typ, Some(var("y"))))]),
        ),
      ),
      test_case(
        "alpha equivalence",
        `Quick,
        match_check(
          ~ctx_in=[("x", (hole_typ, None))],
          "fun u -> (x, u)",
          "fun v -> (y, v)",
          Some([("x", (hole_typ, Some(var("y"))))]),
        ),
      ),
      test_case(
        "Let alpha equivalence",
        `Quick,
        match_check(
          ~ctx_in=[("x", (hole_typ, None))],
          "let u = 1 in x",
          "let v = 1 in 5",
          Some([("x", (hole_typ, Some(int(5))))]),
        ),
      ),
      test_case(
        "Shadowing",
        `Quick,
        match_check(
          ~ctx_in=[("x", (hole_typ, None))],
          "let x = 1 in x",
          "let x = 1 in x",
          Some([("x", (hole_typ, None))]),
        ),
      ),
      test_case(
        "Shadow alpha equivalence",
        `Quick,
        match_check(
          ~ctx_in=[("x", (hole_typ, None))],
          "let x = 1 in x",
          "let y = 1 in y",
          Some([("x", (hole_typ, None))]),
        ),
      ),
      test_case(
        "deep alpha equivalence",
        `Quick,
        match_check(
          ~ctx_in=[("x", (hole_typ, None))],
          "fun (x,y,(z,w)) -> (x,y,z,w)",
          "fun (a,b,(c,d)) -> (a,b,c,d)",
          Some([("x", (hole_typ, None))]),
        ),
      ),
      test_case(
        "ignores casts",
        `Quick,
        match_check("x", "x: Int", Some([])),
      ),
      test_case(
        "Nested function alpha equivalence",
        `Quick,
        match_check(
          ~ctx_in=[("x", (hole_typ, None))],
          "fun u -> fun v -> (x, u, v)",
          "fun a -> fun b -> (y, a, b)",
          Some([("x", (hole_typ, Some(var("y"))))]),
        ),
      ),
      test_case(
        "FixF alpha equivalence",
        `Quick,
        match_check(
          ~ctx_in=[("x", (hole_typ, None))],
          "fix f -> fun n -> x",
          "fix g -> fun m -> y",
          Some([("x", (hole_typ, Some(var("y"))))]),
        ),
      ),
      test_case(
        "Match expression alpha equivalence",
        `Quick,
        match_check(
          ~ctx_in=[("x", (hole_typ, None))],
          "case z | u => (x, u) end",
          "case z | v => (y, v) end",
          Some([("x", (hole_typ, Some(var("y"))))]),
        ),
      ),
      test_case(
        "Match equality with shadowing 1",
        `Quick,
        match_check(
          ~ctx_in=[("x", (hole_typ, None))],
          "x == x",
          "[x] == [x]",
          Some([("x", (hole_typ, Some(list_lit([var("x")]))))]),
        ),
      ),
      test_case(
        "Match equality with shadowing 2",
        `Quick,
        match_check(
          ~ctx_in=[("x", (hole_typ, None))],
          "x == x",
          "x == x",
          Some([("x", (hole_typ, Some(var("x"))))]),
        ),
      ),
    ],
  ),
];

/* ---- Pattern metavariables in target slots (MetaVar / match_pattern) ---
 *
 * `$e` matches any expression, `$v` any value, and both are non-linear
 * (occurrences are independent), exactly as in stepper filters. Named
 * metavariables `$x` are linear: every occurrence must agree.
 */

let pattern_check = (~env=Language.Environment.empty, pat, exp, expected, ()) =>
  check(
    Alcotest.bool,
    pat ++ " matches " ++ exp,
    expected,
    Language.MatchExp.matches_pattern(~env, parse_exp(pat), parse_exp(exp)),
  );

/* `ProofHacks.nth_exp_pat` indexes among pattern matches. Returns the
 * matched subterm rendered back to a string for easy comparison. */
let nth_pat_check = (pat, n, target, expected: option(string), ()) => {
  let matched =
    Language.ProofHacks.nth_exp_pat(parse_exp(pat), n, parse_exp(target));
  switch (expected, matched) {
  | (None, None) =>
    check(
      Alcotest.bool,
      pat ++ " has no match #" ++ string_of_int(n),
      true,
      true,
    )
  | (Some(exp), Some(got)) =>
    if (!Language.Exp.fast_equal(got, parse_exp(exp))) {
      Alcotest.fail(
        pat
        ++ " match #"
        ++ string_of_int(n)
        ++ ": expected "
        ++ exp
        ++ " but got "
        ++ Language.Exp.show(got),
      );
    }
  | (Some(exp), None) =>
    Alcotest.fail("expected match " ++ exp ++ " for " ++ pat)
  | (None, Some(_)) => Alcotest.fail("expected no match for " ++ pat)
  };
};

/* Ground targets must keep behaving exactly as they do today: for a
 * metavariable-free target, the dispatcher used by the checker has to
 * agree with the original `nth_exp` occurrence lookup. */
let ground_regression_check = (target, n, goal, ()) => {
  let target' = parse_exp(target);
  let goal' = parse_exp(goal);
  let old = Language.ProofHacks.nth_exp(target', n, goal');
  let new_ = Language.ProofHacks.nth_exp_target(target', n, goal');
  check(
    Alcotest.bool,
    "ground target `" ++ target ++ "` #" ++ string_of_int(n) ++ " unchanged",
    true,
    Option.equal(Language.Exp.fast_equal, old, new_),
  );
};

let pattern_tests = [
  (
    "MetaVar patterns",
    [
      /* $e: any expression, and non-linear. */
      test_case(
        "$e matches a literal",
        `Quick,
        pattern_check("$e", "1", true),
      ),
      test_case(
        "$e matches a compound expression",
        `Quick,
        pattern_check("$e", "1 + 2", true),
      ),
      test_case(
        "$e == $e matches a reflexive equation",
        `Quick,
        pattern_check("$e == $e", "1 == 1", true),
      ),
      test_case(
        "$e is non-linear: sides need not agree",
        `Quick,
        pattern_check("$e == $e", "1 == 2", true),
      ),
      test_case(
        "$e still respects the surrounding structure",
        `Quick,
        pattern_check("$e == $e", "1 + 2", false),
      ),
      /* $v: any value. */
      test_case(
        "$v matches a value",
        `Quick,
        pattern_check("$v + $v", "1 + 2", true),
      ),
      test_case(
        "$v rejects a non-value operand",
        `Quick,
        pattern_check("$v + $v", "(1 + 2) + 4", false),
      ),
      test_case(
        "$e accepts where $v rejects",
        `Quick,
        pattern_check("$e + $e", "(1 + 2) + 4", true),
      ),
      /* Named metavariables are linear. */
      test_case(
        "$x == $x requires both sides to agree",
        `Quick,
        pattern_check("$x == $x", "1 == 1", true),
      ),
      test_case(
        "$x == $x rejects differing sides",
        `Quick,
        pattern_check("$x == $x", "1 == 2", false),
      ),
      /* Ground patterns are the degenerate case. */
      test_case(
        "a ground pattern matches only itself",
        `Quick,
        pattern_check("1 + 2", "1 + 2", true),
      ),
      test_case(
        "a ground pattern rejects a different term",
        `Quick,
        pattern_check("1 + 2", "1 + 3", false),
      ),
      /* `at <idx>` indexes among pattern matches. */
      /* Two notes on what these expect.
       *
       * 1. The match is the parenthesized node: pattern matching
       *    ignores parens, so the outermost matching node is `(3 + 4)`
       *    itself, which is also the node today's exact-quote lookup
       *    returns.
       * 2. Occurrence order is `Exp.map_term`'s, which reaches a binary
       *    operator's right operand first. `nth_exp_pat` is built on the
       *    very same traversal skeleton as `nth_exp`, so `at <idx>`
       *    counts pattern matches in exactly the order it counts exact
       *    occurrences today. */
      test_case(
        "nth_exp_pat: first $v + $v match",
        `Quick,
        nth_pat_check("$v + $v", 0, "(1 + 2) + (3 + 4)", Some("(3 + 4)")),
      ),
      test_case(
        "nth_exp_pat: second $v + $v match",
        `Quick,
        nth_pat_check("$v + $v", 1, "(1 + 2) + (3 + 4)", Some("(1 + 2)")),
      ),
      test_case(
        "nth_exp_pat: no third match",
        `Quick,
        nth_pat_check("$v + $v", 2, "(1 + 2) + (3 + 4)", None),
      ),
      test_case(
        "nth_exp_pat: $e + $e takes the outermost sum first",
        `Quick,
        nth_pat_check(
          "$e + $e",
          0,
          "(1 + 2) + (3 + 4)",
          Some("(1 + 2) + (3 + 4)"),
        ),
      ),
      /* Ground-target regressions against the pre-existing lookup. */
      test_case(
        "ground regression: occurrence 0",
        `Quick,
        ground_regression_check("1 + 2", 0, "(1 + 2) + (1 + 2) == 6"),
      ),
      test_case(
        "ground regression: occurrence 1",
        `Quick,
        ground_regression_check("1 + 2", 1, "(1 + 2) + (1 + 2) == 6"),
      ),
      test_case(
        "ground regression: missing occurrence",
        `Quick,
        ground_regression_check("9 + 9", 0, "(1 + 2) + (1 + 2) == 6"),
      ),
    ],
  ),
];
