open Alcotest;

open Language;
open IdTagged.FreshGrammar;

let settings = CoreSettings.on;
let env = Environment.empty;

let plus = (left, right) =>
  Exp.bin_op(Operators.Int(Operators.Plus), left, right);

let minus = (left, right) =>
  Exp.bin_op(Operators.Int(Operators.Minus), left, right);

let times = (left, right) =>
  Exp.bin_op(Operators.Int(Operators.Times), left, right);

let check_written = (name, left, right, expected) =>
  check(
    option(string),
    name,
    expected,
    Web.RewriteChecker.check_written_step(~settings, ~env, left, right),
  );

let require_written_result = (left, right) =>
  switch (
    Web.RewriteChecker.check_written_step_result(~settings, ~env, left, right)
  ) {
  | Some(result) => result
  | None => fail("expected written step to be accepted")
  };

let rewrite_group_name = (group: Axioms.rewrite_group) => group.name;

let rewrite_rule_id = (rule: Axioms.rewrite_rule) => rule.id;

let prover_hint_prover = (hint: Axioms.prover_hint) => hint.prover;

let has_lean_hint = (rule: Axioms.rewrite_rule) =>
  rule.prover_hints |> List.exists(hint => prover_hint_prover(hint) == "lean");

let has_trace_rule = (rule_id, result: Web.RewriteChecker.check_result) =>
  result.trace |> List.exists(rule => rewrite_rule_id(rule) == rule_id);

let check_simplifies = (name, input, expected) =>
  switch (Web.RewriteChecker.simplify_arithmetic(~settings, ~env, input)) {
  | Some(actual) =>
    check(bool, name, true, Language.Exp.fast_equal(expected, actual))
  | None => fail(name ++ " did not simplify")
  };

let tests = (
  "RewriteChecker",
  [
    test_case("affine commutes variable addition", `Quick, () =>
      check_written(
        "3 + x = x + 3",
        plus(Exp.int(3), Exp.var("x")),
        plus(Exp.var("x"), Exp.int(3)),
        Some("arithmetic"),
      )
    ),
    test_case("affine folds and reorders constants", `Quick, () =>
      check_written(
        "(1 + 2) + x = x + 3",
        plus(plus(Exp.int(1), Exp.int(2)), Exp.var("x")),
        plus(Exp.var("x"), Exp.int(3)),
        Some("arithmetic"),
      )
    ),
    test_case("affine scales symbolic terms by constants", `Quick, () =>
      check_written(
        "2 * x + 3 = x + x + 3",
        plus(times(Exp.int(2), Exp.var("x")), Exp.int(3)),
        plus(plus(Exp.var("x"), Exp.var("x")), Exp.int(3)),
        Some("arithmetic"),
      )
    ),
    test_case("non-affine multiplication is rejected", `Quick, () =>
      check_written(
        "x * x != x + x",
        times(Exp.var("x"), Exp.var("x")),
        plus(Exp.var("x"), Exp.var("x")),
        None,
      )
    ),
    test_case("affine subtraction normalizes coefficients", `Quick, () =>
      check_written(
        "x + 3 - x = 3",
        minus(plus(Exp.var("x"), Exp.int(3)), Exp.var("x")),
        Exp.int(3),
        Some("arithmetic"),
      )
    ),
    test_case("affine simplifier produces grouped expression", `Quick, () =>
      check_simplifies(
        "1 + 2 + x -> x + 3",
        plus(plus(Exp.int(1), Exp.int(2)), Exp.var("x")),
        plus(Exp.var("x"), Exp.int(3)),
      )
    ),
    test_case("affine simplifier groups coefficients", `Quick, () =>
      check_simplifies(
        "x + x + x + 10 -> 3 * x + 10",
        plus(
          plus(plus(Exp.var("x"), Exp.var("x")), Exp.var("x")),
          Exp.int(10),
        ),
        plus(times(Exp.int(3), Exp.var("x")), Exp.int(10)),
      )
    ),
    test_case(
      "affine result names arithmetic group",
      `Quick,
      () => {
        let result =
          require_written_result(
            plus(Exp.int(3), Exp.var("x")),
            plus(Exp.var("x"), Exp.int(3)),
          );
        check(string, "justification", "arithmetic", result.justification);
        check(
          option(string),
          "group name",
          Some("arithmetic"),
          result.group |> Option.map(rewrite_group_name),
        );
        check(bool, "exportable", true, result.exportable);
        check(
          bool,
          "trace has commutativity",
          true,
          has_trace_rule("arith.add_comm", result),
        );
        check(
          bool,
          "trace has collection",
          true,
          has_trace_rule("arith.collect_like_terms", result),
        );
        check(
          bool,
          "trace rules have Lean hints",
          true,
          result.trace |> List.for_all(has_lean_hint),
        );
      },
    ),
    test_case(
      "evaluation equality fallback is not exportable",
      `Quick,
      () => {
        let result = require_written_result(Exp.bool(true), Exp.bool(true));
        check(
          string,
          "justification",
          "same evaluated result",
          result.justification,
        );
        check(
          option(string),
          "no group",
          None,
          result.group |> Option.map(rewrite_group_name),
        );
        check(bool, "not exportable", false, result.exportable);
        check(int, "no trace", 0, result.trace |> List.length);
      },
    ),
  ],
);
