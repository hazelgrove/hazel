open Alcotest;

/* Tests for AntiUnify: most specific generalization of two expressions,
 * and the most general pattern that still pins down one occurrence in a
 * goal. Named Test_AntiUnify to stay clear of Test_Generalize, which
 * covers the unrelated `generalize` proof step. */

let parse_exp = (s: string) =>
  switch (Haz3lcore.Parser.to_term(s, ~root=Exp)) {
  | Some(e) => e
  | None => Alcotest.fail("Failed to parse expression: " ++ s)
  };

/* Only ever printed on failure, so the verbose derived printer is fine. */
let show = (e: Language.Exp.t): string => Language.Exp.show(e);

/* ---- msg (anti-unification) ---------------------------------------- */

let msg_check = (e1: string, e2: string, expected: string, ()) => {
  let got = Language.AntiUnify.msg(parse_exp(e1), parse_exp(e2));
  if (!Language.Exp.fast_equal(got, parse_exp(expected))) {
    Alcotest.fail(
      "msg("
      ++ e1
      ++ ", "
      ++ e2
      ++ "): expected "
      ++ expected
      ++ " but got "
      ++ show(got),
    );
  };
};

/* The defining property: whatever msg returns must match both inputs.
 * Checked separately from the exact shape, so a pair whose ideal
 * generalization is debatable still gets a meaningful assertion. */
let msg_generalizes = (e1: string, e2: string, ()) => {
  let (x1, x2) = (parse_exp(e1), parse_exp(e2));
  let got = Language.AntiUnify.msg(x1, x2);
  check(
    Alcotest.bool,
    "msg(" ++ e1 ++ ", " ++ e2 ++ ") matches both inputs",
    true,
    Language.MatchExp.matches_pattern(got, x1)
    && Language.MatchExp.matches_pattern(got, x2),
  );
};

/* ---- discriminating_pattern ---------------------------------------- */

/* Locate the `n`th occurrence of `sub` in `goal`, ask AntiUnify for a
 * discriminating pattern for it, and check both the pattern's shape and
 * that resolving (pattern, index) really lands back on that same node —
 * the round-trip a GUI would depend on. */
let discriminating_check =
    (
      ~goal: string,
      ~sub: string,
      ~occurrence: int=0,
      ~expected_pattern: string,
      ~expected_index: int,
      (),
    ) => {
  let goal' = parse_exp(goal);
  let target =
    switch (Language.ProofHacks.nth_exp(parse_exp(sub), occurrence, goal')) {
    | Some(e) => e
    | None => Alcotest.fail("test setup: " ++ sub ++ " not found in " ++ goal)
    };
  let id = Language.Exp.rep_id(target);
  let (pat, idx) =
    Language.AntiUnify.discriminating_pattern(~goal=goal', id);
  if (!Language.Exp.fast_equal(pat, parse_exp(expected_pattern))) {
    Alcotest.fail(
      "pattern for "
      ++ sub
      ++ " in "
      ++ goal
      ++ ": expected "
      ++ expected_pattern
      ++ " but got "
      ++ show(pat),
    );
  };
  check(
    Alcotest.int,
    "index for " ++ sub ++ " in " ++ goal,
    expected_index,
    idx,
  );
  /* Round-trip: the pattern and index must resolve to the same node. */
  switch (Language.ProofHacks.nth_exp_pat(pat, idx, goal')) {
  | Some(found) =>
    check(
      Alcotest.bool,
      "(pattern, index) resolves back to the target",
      true,
      Language.Exp.rep_id(found) == id,
    )
  | None => Alcotest.fail("(pattern, index) resolved to nothing")
  };
};

let tests = [
  (
    "AntiUnify.msg",
    [
      test_case(
        "identical expressions generalize to themselves",
        `Quick,
        msg_check("1 + 2", "1 + 2", "1 + 2"),
      ),
      test_case(
        "one differing operand becomes one metavariable",
        `Quick,
        msg_check("1 + 2", "1 + 3", "1 + $x1"),
      ),
      test_case(
        "two independent disagreements get distinct metavariables",
        `Quick,
        msg_check("1 + 2", "3 + 4", "$x1 + $x2"),
      ),
      test_case(
        "a recurring disagreement shares one metavariable (least general)",
        `Quick,
        msg_check("1 + 1", "2 + 2", "$x1 + $x1"),
      ),
      test_case(
        "reflexive equations generalize to a reflexive pattern",
        `Quick,
        msg_check("1 == 1", "2 == 2", "$x1 == $x1"),
      ),
      test_case(
        "non-reflexive equations do not",
        `Quick,
        msg_check("1 == 2", "3 == 4", "$x1 == $x2"),
      ),
      test_case(
        "differing heads generalize to a bare metavariable",
        `Quick,
        msg_check("1 + 2", "true", "$x1"),
      ),
      test_case(
        "differing operators do not decompose",
        `Quick,
        msg_check("1 + 2", "1 - 2", "$x1"),
      ),
      test_case(
        "structure above the disagreement is kept",
        `Quick,
        msg_check("(1 + 2) + 9", "(1 + 3) + 9", "(1 + $x1) + 9"),
      ),
      test_case(
        "list literals generalize elementwise",
        `Quick,
        msg_check("[1, 2]", "[1, 3]", "[1, $x1]"),
      ),
      test_case(
        "bound variables correspond up to alpha",
        `Quick,
        msg_check("fun u -> u + 1", "fun v -> v + 2", "fun u -> u + $x1"),
      ),
      test_case(
        "free variables of the same name are kept",
        `Quick,
        msg_check("x + 1", "x + 2", "x + $x1"),
      ),
      test_case(
        "free variables of different names are generalized",
        `Quick,
        msg_check("x + 1", "y + 1", "$x1 + 1"),
      ),
      /* The defining property, over a spread of shapes. */
      test_case(
        "generalizes both: nested arithmetic",
        `Quick,
        msg_generalizes("(1 + 2) * 3", "(4 + 5) * 6"),
      ),
      test_case(
        "generalizes both: conditional",
        `Quick,
        msg_generalizes("if true then 1 else 2", "if false then 3 else 2"),
      ),
      test_case(
        "generalizes both: let binding",
        `Quick,
        msg_generalizes("let a = 1 in a + 1", "let b = 2 in b + 2"),
      ),
      test_case(
        "generalizes both: application",
        `Quick,
        msg_generalizes("f(1, 2)", "f(3, 4)"),
      ),
      test_case(
        "generalizes both: unrelated shapes",
        `Quick,
        msg_generalizes("fun u -> u", "[1, 2, 3]"),
      ),
    ],
  ),
  (
    "AntiUnify.discriminating_pattern",
    [
      /* `1 + 2` is the only sum in the goal, so the shape `$e + $e` —
       * one rung more general than the ground quote — already pins it
       * down, and no index is needed. */
      test_case(
        "a unique shape needs no index",
        `Quick,
        discriminating_check(
          ~goal="1 + 2 == 3",
          ~sub="1 + 2",
          ~expected_pattern="$e + $e",
          ~expected_index=0,
        ),
      ),
      /* Two identical sums: no rung of the ladder is unambiguous, so we
       * keep the ground term and disambiguate with the index. The index
       * follows the same traversal order `at <idx>` uses, which reaches
       * a binary operator's right operand first. */
      test_case(
        "a repeated term falls back to the ground quote plus an index",
        `Quick,
        discriminating_check(
          ~goal="(1 + 2) + (1 + 2) == 6",
          ~sub="1 + 2",
          ~occurrence=0,
          ~expected_pattern="1 + 2",
          ~expected_index=0,
        ),
      ),
      test_case(
        "the second occurrence of a repeated term gets index 1",
        `Quick,
        discriminating_check(
          ~goal="(1 + 2) + (1 + 2) == 6",
          ~sub="1 + 2",
          ~occurrence=1,
          ~expected_pattern="1 + 2",
          ~expected_index=1,
        ),
      ),
      /* Distinct sums: the generic shape is ambiguous, so the ladder
       * backs off one rung to the ground term, which is unique — index
       * 0, no ambiguity. */
      test_case(
        "backs off one rung when the generic shape is ambiguous",
        `Quick,
        discriminating_check(
          ~goal="(1 + 2) + (3 + 4) == 10",
          ~sub="1 + 2",
          ~expected_pattern="1 + 2",
          ~expected_index=0,
        ),
      ),
      /* A leaf: the ladder for a single node is just `$e` and the node
       * itself, so a literal appearing once is identified by itself. */
      test_case(
        "a unique literal is identified by itself",
        `Quick,
        discriminating_check(
          ~goal="7 + 2 == 9",
          ~sub="7",
          ~expected_pattern="7",
          ~expected_index=0,
        ),
      ),
    ],
  ),
];
