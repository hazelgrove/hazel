open Alcotest;
open Web;

/* `CodeGrading` turns a student's work into the four component scores of a code
 * exercise. `Test_GradingReport` covers the outer grade (max points, percent to
 * score); the per-component arithmetic here was uncovered.
 *
 * Two things are worth pinning, and they are different in kind:
 *
 *  - `SyntaxReport.mk` pairs each syntax-test result with the hint shown to the
 *    student. The score being right is not enough -- a result attached to the
 *    wrong hint tells the student to fix something that is not wrong. It pairs
 *    with `List.map2`, which also raises if the two lists ever diverge.
 *  - `MutationTestingReport.percentage` is the fraction of planted bugs the
 *    student's tests caught, with a guard for the no-bugs case. Off-by-one here
 *    moves marks. */

let editor_of = (text: string): Haz3lcore.Editor.t =>
  switch (Haz3lcore.Parser.to_zipper(~root=Haz3lcore.Sort.Exp, text)) {
  | None => Alcotest.fail("could not parse: " ++ text)
  | Some(z) => Haz3lcore.Editor.Model.mk(z, ~root=Haz3lcore.Sort.Exp)
  };

/* A recursive-but-not-tail-recursive implementation, so the three predicates
   below have distinct answers and a mispairing is visible. */
let impl =
  editor_of({|let f = fun x -> if x == 0 then 0 else 1 + f(x - 1) in f(3)|});

let tests_spec: CodeExercise.syntax_tests = [
  ("must be recursive", IsRecursive("f")),
  ("must be tail recursive", IsTailRecursive("f")),
  ("must not be recursive", IsNotRecursive("f")),
];

let approx = (a, b) => Float.abs(a -. b) < 0.001;

let mutation_report = statuses =>
  CodeGrading.MutationTestingReport.{
    results:
      List.mapi((i, s) => (s, "bug " ++ string_of_int(i)), statuses),
  };

let percentage_of = r => CodeGrading.MutationTestingReport.percentage(r);

let tests = (
  "CodeGrading",
  [
    /* The pairing, not just the score: recursive passes, tail-recursive fails,
       not-recursive fails, and each verdict must sit against its own hint. */
    test_case(
      "SyntaxReport pairs each result with its own hint",
      `Quick,
      () => {
        let r =
          CodeGrading.SyntaxReport.mk(~your_impl=impl, ~tests=tests_spec);
        check(
          list(pair(bool, string)),
          "hinted results",
          [
            (true, "must be recursive"),
            (false, "must be tail recursive"),
            (false, "must not be recursive"),
          ],
          r.hinted_results,
        );
        check(bool, "one of three", true, approx(r.percentage, 1. /. 3.));
      },
    ),
    /* An exercise with no syntax requirements must not divide by zero, and must
       not silently cost the student marks either. */
    test_case(
      "SyntaxReport with no tests gives full marks",
      `Quick,
      () => {
        let r = CodeGrading.SyntaxReport.mk(~your_impl=impl, ~tests=[]);
        check(int, "no hinted results", 0, List.length(r.hinted_results));
        check(bool, "full marks", true, approx(r.percentage, 1.));
      },
    ),
    test_case(
      "mutation score is the fraction of bugs caught",
      `Quick,
      () => {
        check(
          bool,
          "two of four",
          true,
          approx(
            percentage_of(
              mutation_report(
                [Pass, Fail, Pass, Fail]: list(Language.TestStatus.t),
              ),
            ),
            0.5,
          ),
        );
        check(
          bool,
          "all caught",
          true,
          approx(
            percentage_of(
              mutation_report([Pass, Pass]: list(Language.TestStatus.t)),
            ),
            1.,
          ),
        );
        check(
          bool,
          "none caught",
          true,
          approx(
            percentage_of(
              mutation_report([Fail, Fail]: list(Language.TestStatus.t)),
            ),
            0.,
          ),
        );
      },
    ),
    /* Only `Pass` counts as catching a bug: an indeterminate test result must
       not be credited as if the student had caught it. */
    test_case("an indeterminate result does not count as caught", `Quick, () =>
      check(
        bool,
        "one of two",
        true,
        approx(
          percentage_of(
            mutation_report([Pass, Indet]: list(Language.TestStatus.t)),
          ),
          0.5,
        ),
      )
    ),
    test_case("no planted bugs gives full marks", `Quick, () =>
      check(
        bool,
        "vacuous pass",
        true,
        approx(percentage_of(mutation_report([])), 1.),
      )
    ),
  ],
);
