open Alcotest;

/* `ProofRule.mentions_any` answers "does this fact mention x free?" for the
 * prover's capture tests (`ProofCtx.of_env`/`of_ctx` `is_captured`,
 * `ProofCheck`'s generalize). It used to answer by running FULL statics on
 * the rule's core and reading the co-context back out — the deepest
 * recursion in the evaluation phase, and enough to overflow the stack a
 * browser gives a worker on docs/stlc-progress-example.hazel (see
 * `test/run_node.sh` for why the suite doesn't see stack limits).
 *
 * These pin the direct walk that replaced it: occurrence, shadowing by
 * every binding form the walk special-cases, and the reach into
 * assumptions and into expressions nested under types. */

let parse_exp = (s: string) =>
  switch (Haz3lcore.Parser.to_term(s, ~root=Exp)) {
  | Some(e) => e
  | None => Alcotest.fail("Failed to parse expression: " ++ s)
  };

let mentions = (name: string, src: string): bool =>
  Language.ProofRule.mentions_any(
    Language.ProofRule.exp_to_rule(parse_exp(src)),
    [name],
  );

let check_mentions = (~expected: bool, name, src, ()) =>
  check(
    bool,
    (expected ? "mentions " : "does not mention ")
    ++ name
    ++ ": "
    ++ src,
    expected,
    mentions(name, src),
  );

let tests = [
  (
    "ProofRule.mentions_any",
    [
      test_case(
        "free occurrence in the conclusion",
        `Quick,
        check_mentions(~expected=true, "x", "x == 1"),
      ),
      test_case(
        "absent name",
        `Quick,
        check_mentions(~expected=false, "y", "x == 1"),
      ),
      test_case(
        "free occurrence in an assumption",
        `Quick,
        check_mentions(~expected=true, "x", "x == 1 ==> 2 == 2"),
      ),
      test_case(
        "the rule's own forall binder is not a mention",
        `Quick,
        check_mentions(~expected=false, "x", "forall x: Int -> x == 1"),
      ),
      test_case(
        "a rule binder shadows only itself",
        `Quick,
        check_mentions(~expected=true, "y", "forall x: Int -> x == y"),
      ),
      test_case(
        "shadowed by an inner fun binder",
        `Quick,
        check_mentions(~expected=false, "x", "(fun x -> x)(1) == 1"),
      ),
      test_case(
        "free under an inner fun binder",
        `Quick,
        check_mentions(~expected=true, "x", "(fun y -> x)(1) == 1"),
      ),
      test_case(
        "shadowed by an inner let binder",
        `Quick,
        check_mentions(~expected=false, "x", "(let x = 1 in x) == 1"),
      ),
      test_case(
        "free in an inner let's body",
        `Quick,
        check_mentions(~expected=true, "x", "(let y = 1 in x) == 1"),
      ),
      test_case(
        "shadowed by a case branch's pattern",
        `Quick,
        check_mentions(
          ~expected=false,
          "x",
          "(case 1 | x => x end) == 1",
        ),
      ),
      test_case(
        "free in a case scrutinee that a branch shadows",
        `Quick,
        check_mentions(~expected=true, "x", "(case x | x => x end) == 1"),
      ),
      test_case(
        "shadowed by an inner forall",
        `Quick,
        check_mentions(
          ~expected=false,
          "x",
          "(forall x: Int -> x == 1) == true",
        ),
      ),
    ],
  ),
];
