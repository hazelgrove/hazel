open Alcotest;
open Web;

/* `SyntaxTest` decides whether a student's submission satisfies an exercise's
 * syntactic requirements, and it had zero coverage. A wrong answer here marks a
 * correct submission wrong (or passes an incorrect one), silently -- the student
 * is the one who finds out.
 *
 * The predicates are pure functions of `Exp.t`, so they can be tested against
 * real programs with no setup. `is_tail_recursive` is the subtle one and gets
 * most of the attention: a recursive call in a non-tail position has to be
 * rejected, and the difference between the two is a single operator. */

let parse = (s: string): Language.Exp.t =>
  switch (Haz3lcore.Parser.to_term(s, ~root=Exp)) {
  | Some(e) => e
  | None => Alcotest.fail("failed to parse: " ++ s)
  };

let pred = (p, program) => SyntaxTest.predicate_fn(p, parse(program));

/* Fixtures, so each is named once and the intent is legible. */
let tail_rec = {|let f = fun x -> if x == 0 then 0 else f(x - 1) in f(3)|};
let non_tail_rec = {|let f = fun x -> if x == 0 then 0 else 1 + f(x - 1) in f(3)|};
let not_rec = {|let f = fun x -> x + 1 in f(3)|};
/* `f` is FREE here. That matters: these predicates respect shadowing, so a
   program that rebinds `f` reports no mention of it -- which is what makes
   `is_recursive` work (it asks whether the definition mentions its own name).
   A fixture that wrote `let f = ... in let g = f in 0` would report false. */
let mentioned_not_applied = {|let g = f in 0|};

let tests = (
  "SyntaxTest",
  [
    test_case(
      "is_recursive distinguishes recursive from not",
      `Quick,
      () => {
        check(
          bool,
          "self-calling function",
          true,
          pred(IsRecursive("f"), tail_rec),
        );
        check(
          bool,
          "non-tail recursion is still recursion",
          true,
          pred(IsRecursive("f"), non_tail_rec),
        );
        check(bool, "no self-call", false, pred(IsRecursive("f"), not_rec));
      },
    ),
    test_case(
      "IsNotRecursive is the negation",
      `Quick,
      () => {
        check(
          bool,
          "on a recursive fn",
          false,
          pred(IsNotRecursive("f"), tail_rec),
        );
        check(
          bool,
          "on a plain fn",
          true,
          pred(IsNotRecursive("f"), not_rec),
        );
      },
    ),
    /* The one that costs a student marks if it is wrong. `1 + f(x - 1)` differs
       from `f(x - 1)` by one operator, and only the second is a tail call. */
    test_case(
      "is_tail_recursive rejects a call under an operator",
      `Quick,
      () => {
        check(
          bool,
          "tail call accepted",
          true,
          pred(IsTailRecursive("f"), tail_rec),
        );
        check(
          bool,
          "call under + rejected",
          false,
          pred(IsTailRecursive("f"), non_tail_rec),
        );
      },
    ),
    test_case(
      "var_applied needs an application, not a mention",
      `Quick,
      () => {
        check(bool, "f is called", true, pred(VarApplied("f"), tail_rec));
        check(
          bool,
          "f is only bound to g",
          false,
          pred(VarApplied("f"), mentioned_not_applied),
        );
      },
    ),
    test_case("var_mention sees a mention without an application", `Quick, () =>
      check(
        bool,
        "f is mentioned",
        true,
        SyntaxTest.var_mention("f", parse(mentioned_not_applied)),
      )
    ),
    test_case("var_mention is false for an absent name", `Quick, () =>
      check(
        bool,
        "no such variable",
        false,
        SyntaxTest.var_mention("zz", parse(not_rec)),
      )
    ),
    /* Shadowing, pinned because everything else depends on it: a mention that
       refers to a fresh binding of the same name is not a mention of the name
       being graded. This is why `is_recursive` can be written in terms of
       `var_mention` at all. */
    test_case(
      "var_mention respects shadowing",
      `Quick,
      () => {
        check(
          bool,
          "rebound name is not a mention",
          false,
          SyntaxTest.var_mention("x", parse({|let x = 1 in x|})),
        );
        check(
          bool,
          "a free name is a mention",
          true,
          SyntaxTest.var_mention("x", parse({|let y = 1 in x|})),
        );
        check(
          bool,
          "a function parameter shadows too",
          false,
          SyntaxTest.var_mention("x", parse({|fun x -> x|})),
        );
      },
    ),
    /* `check` is what the grader calls; the percentage is what lands in the
       student's score, so the aggregation is worth pinning alongside it. */
    test_case(
      "check reports one result per predicate",
      `Quick,
      () => {
        let r =
          SyntaxTest.check(
            parse(tail_rec),
            [
              SyntaxTest.predicate_fn(IsRecursive("f")),
              SyntaxTest.predicate_fn(IsTailRecursive("f")),
              SyntaxTest.predicate_fn(IsNotRecursive("f")),
            ],
          );
        check(list(bool), "results", [true, true, false], r.results);
        check(
          bool,
          "two of three",
          true,
          Float.abs(r.percentage -. 2. /. 3.) < 0.001,
        );
      },
    ),
    /* Documented behaviour that is easy to "fix" into a division by zero: an
       exercise with no syntax requirements passes vacuously. */
    test_case(
      "check with no predicates passes vacuously",
      `Quick,
      () => {
        let r = SyntaxTest.check(parse(not_rec), []);
        check(list(bool), "no results", [], r.results);
        check(bool, "full marks", true, r.percentage == 1.);
      },
    ),
  ],
);
