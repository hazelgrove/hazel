open Alcotest;
open Haz3lcore;
open Language;
open Web;

/* The bundled code and theorem exercises are .hzt text
   (hazel-programs/exercises/), parsed by ExerciseText at startup. */

let text = (z: Zipper.t): string => MarkerParse.to_text(z);

/* An exercise, editors as their text: two exercises with the same
   description load the same cells with the same programs. */
let describe = (e: Exercise.t): string =>
  switch (e) {
  | Code(s) =>
    CodeExercise.show_p((fmt, z) => Format.fprintf(fmt, "%S", text(z)), s)
  | Theorem(s) =>
    Printf.sprintf(
      "{id=%s; title=%S; module_name=%S; prompt=%S; max_points=%d; prelude=%S; lemmas=%S; theorem=%S}",
      Id.to_string(s.id),
      s.title,
      s.module_name,
      s.prompt,
      s.max_points,
      text(s.prelude),
      text(s.lemmas),
      text(s.theorem),
    )
  | Derivation(s) => "derivation " ++ s.title
  };

let to_text = (e: Exercise.t): string =>
  switch (ExerciseText.to_file(e)) {
  | Some((_, t)) => t
  | None => fail("no text form")
  };

let bundled =
  ExerciseSettings_base.sources
  |> List.map(((rel, src)) => (rel, src, ExerciseText.of_text(~rel, src)));

let code_specs =
  List.filter_map(
    ((rel, _, e: Exercise.t)) =>
      switch (e) {
      | Code(s) => Some((rel, s))
      | _ => None
      },
    bundled,
  );

/* ---------- grading ---------- */

let evaluate = (term: Exp.t): option(TestResults.t) =>
  switch (
    term
    |> CachedStatics.init_from_term(
         ~settings=CoreSettings.on,
         ~is_dynamic_term=false,
       )
    |> ((x: CachedStatics.t) => x.elaborated)
    |> Evaluator.evaluate_and_limit(
         ~step_limit=1000000,
         ~env=Builtins.env_init,
       )
  ) {
  | StepLimitExceeded => None
  | LimitedCompleted((_, state)) =>
    Some(state |> EvaluatorState.get_tests |> TestResults.mk_results)
  };

/* The grading report of a spec as the student-facing grading panel (and
   the CLI's grade-report) computes it. */
let grade = (s: CodeExercise.spec): CodeGrading.GradingReport.t => {
  let eds = CodeExercise.eds_of_spec(s, ~settings=CoreSettings.on);
  let stitched_tests =
    CodeExercise.stitch_term(eds)
    |> CodeExercise.map_stitched((_, item: CodeExercise.TermItem.t) =>
         evaluate(item.term)
       );
  CodeGrading.GradingReport.mk(eds, ~stitched_tests);
};

let show_status = TestStatus.show;

/* Everything the grading panel reports, as text. */
let describe_grade = (s: CodeExercise.spec): string => {
  let r = grade(s);
  let results = (t: option(TestResults.t)) =>
    switch (t) {
    | None => "none"
    | Some(t) =>
      Printf.sprintf(
        "%d/%d passing, %d failing",
        t.passing,
        t.total,
        t.failing,
      )
    };
  let (earned, max) = CodeGrading.GradingReport.overall_score(r);
  Printf.sprintf(
    "validation: %s (%.3f)\nmutation: [%s] (%.3f)\nsyntax: [%s] (%.3f)\nimpl: [%s] (%.3f)\noverall: %.2f / %.2f",
    results(r.test_validation_report.test_results),
    CodeGrading.TestValidationReport.percentage(r.test_validation_report),
    r.mutation_testing_report.results
    |> List.map(((st, h)) => show_status(st) ++ " " ++ h)
    |> String.concat("; "),
    CodeGrading.MutationTestingReport.percentage(r.mutation_testing_report),
    r.syntax_report.hinted_results
    |> List.map(((ok, h)) => string_of_bool(ok) ++ " " ++ h)
    |> String.concat("; "),
    r.syntax_report.percentage,
    r.impl_grading_report.hinted_results
    |> List.map(((st, h)) => show_status(st) ++ " " ++ h)
    |> String.concat("; "),
    CodeGrading.ImplGradingReport.percentage(
      r.impl_grading_report,
      r.syntax_report,
    ),
    earned,
    max,
  );
};

/* A student who submits the solution, with the hidden tests as their own. */
let model_submission = (s: CodeExercise.spec): CodeExercise.spec => {
  ...s,
  your_impl: s.correct_impl,
  your_tests: {
    ...s.your_tests,
    tests: s.hidden_tests.tests,
  },
};

/* The same student, submitting hidden bug `i` instead. */
let buggy_submission = (s: CodeExercise.spec, i: int): CodeExercise.spec => {
  ...model_submission(s),
  your_impl: List.nth(s.hidden_bugs, i).impl,
};

let grading_tests =
  code_specs
  |> List.concat_map(((rel, s: CodeExercise.spec)) =>
       [
         test_case(
           rel ++ ": the solution earns every point",
           `Quick,
           () => {
             let r = grade(model_submission(s));
             let (earned, max) = CodeGrading.GradingReport.overall_score(r);
             check(
               bool,
               describe_grade(model_submission(s)),
               true,
               max > 0. && Float.equal(earned, max),
             );
           },
         ),
         test_case(
           rel ++ ": the hidden tests catch every hidden bug", `Quick, () =>
           List.iteri(
             (i, b: CodeExercise.wrong_impl(Zipper.t)) =>
               switch (
                 grade(buggy_submission(s, i)).impl_grading_report.
                   test_results
               ) {
               | None => fail(b.hint ++ ": no test results")
               | Some(t) =>
                 check(
                   bool,
                   b.hint ++ " fails a hidden test",
                   true,
                   t.failing > 0,
                 )
               },
             s.hidden_bugs,
           )
         ),
       ]
     );

/* ---------- loading and round trips ---------- */

let loading_tests =
  bundled
  |> List.concat_map(((rel, src, e)) =>
       [
         /* The file is what printing its exercise gives: every editor
            loads as the program written in it, indentation included, and a
            file exported from the app reads like a committed one. */
         test_case(rel ++ ": prints back to its source", `Quick, () =>
           check(string, rel, src, to_text(e))
         ),
         test_case(rel ++ ": the id is the file's id= flag", `Quick, () =>
           check(
             bool,
             rel,
             true,
             Util.StringUtil.match(
               Util.StringUtil.regexp(
                 "id=" ++ Id.to_string(Exercise.id_of(e)),
               ),
               src,
             ),
           )
         ),
       ]
     );

let fails_to_load = (src: string): bool =>
  switch (ExerciseText.of_text(~rel="t.hzt", src)) {
  | _ => false
  | exception (Failure(_)) => true
  };

let blank =
  CodeExercise.blank_spec(
    ~title="Blank",
    ~module_name="Ex_Blank",
    ~point_distribution={
      test_validation: 1,
      mutation_testing: 2,
      impl_grading: 3,
    },
    ~required_tests=4,
    ~provided_tests=1,
    ~num_wrong_impls=2,
  );

let format_tests = [
  test_case(
    "a spec round-trips through text",
    `Quick,
    () => {
      let spec: CodeExercise.spec = {
        ...blank,
        module_name: "My_Odd_Name",
        prompt: "# Heading\n\nSome *markdown*.",
        syntax_tests: [
          ("f is recursive", IsRecursive("f")),
          ("g is not", IsNotRecursive("g")),
          ("h is tail recursive", IsTailRecursive("h")),
          ("map is used", VarApplied("map")),
        ],
        hidden_tests: {
          ...blank.hidden_tests,
          hints: ["first", "second"],
        },
      };
      let e = Exercise.Code(spec);
      check(
        string,
        "describe",
        describe(e),
        describe(ExerciseText.of_text(~rel="my-odd-name.hzt", to_text(e))),
      );
    },
  ),
  test_case(
    "a theorem exercise round-trips through text",
    `Quick,
    () => {
      let e =
        Exercise.Theorem(
          TheoremExercise.blank_spec(~title="T", ~module_name="Ex_T"),
        );
      check(
        string,
        "describe",
        describe(e),
        describe(ExerciseText.of_text(~rel="t.hzt", to_text(e))),
      );
    },
  ),
  test_case(
    "the filename gives the module name, and back",
    `Quick,
    () => {
      check(
        string,
        "module_name_of",
        "Ex_OddlyRecursive",
        ExerciseText.module_name_of("oddly-recursive.hzt"),
      );
      check(
        string,
        "filename_of",
        "oddly-recursive.hzt",
        ExerciseText.filename_of("Ex_OddlyRecursive"),
      );
    },
  ),
  test_case("sections parse in any order; bugs keep theirs", `Quick, () =>
    switch (
      ExerciseText.of_text(
        ~rel="t.hzt",
        "@bug second\n1\n@flags\nid=3335e34d-d211-4332-91e2-815e9e183885\n@title\nT\n@bug first\n2\n",
      )
    ) {
    | Code(s) =>
      check(
        list(string),
        "hints",
        ["second", "first"],
        List.map(
          (b: CodeExercise.wrong_impl(Zipper.t)) => b.hint,
          s.hidden_bugs,
        ),
      )
    | _ => fail("expected a code exercise")
    }
  ),
  test_case(
    "malformed sources fail to load",
    `Quick,
    () => {
      check(bool, "no id", true, fails_to_load("@title\nT\n"));
      check(
        bool,
        "unknown flag",
        true,
        fails_to_load(
          "@title\nT\n@flags\nid=3335e34d-d211-4332-91e2-815e9e183885 requried_tests=3\n",
        ),
      );
      check(
        bool,
        "a code section in a theorem exercise",
        true,
        fails_to_load(
          "@title\nT\n@flags\nid=3335e34d-d211-4332-91e2-815e9e183885\n@theorem\n¿\n@bug b\n¿\n",
        ),
      );
      check(
        bool,
        "bad predicate",
        true,
        fails_to_load(
          "@title\nT\n@flags\nid=3335e34d-d211-4332-91e2-815e9e183885\n@syntax h\nIsRecursive f\n",
        ),
      );
    },
  ),
];

let tests = [
  ("ExerciseText.format", format_tests),
  ("ExerciseText.bundled", loading_tests),
  ("ExerciseText.grading", grading_tests),
];
