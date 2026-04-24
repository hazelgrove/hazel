open Haz3lcore;
open Util;
open Core;
open Language;
open Web.CodeExercise;
open Web.CodeGrading;
open Web.Specs;
open Web.Export;
open Web.ExercisesMode;

/* Batch grading entry point. Invoked as an executable
   (_build/default/src/web/gradingReport.bc.js) by [src/grading/grade/grade.py]
   (see `make grade-json` / `make grade-report`) to turn a persisted exercise
   export into a list of per-exercise grading sections.

   For each exercise in the list, we dispatch on the exercise kind to
   produce a [section]. Code exercises run the full automated grading
   pipeline; Derivation and Theorem exercises currently emit a placeholder
   report (basic support) until richer automated grading is wired up. */

[@deriving (sexp, yojson)]
type item = {
  max: int,
  percentage,
  src: string,
};
let item_to_summary = (name, {max, percentage, src}) =>
  Printf.sprintf(
    "%s: %.1f/%.1f\n\n",
    name,
    percentage *. float_of_int(max),
    float_of_int(max),
  )
  ++ (
    if (String.equal(src, "")) {
      "";
    } else {
      "Source Code:\n\n" ++ src ++ "\n\n";
    }
  );
[@deriving (sexp, yojson)]
type report = {
  summary: string,
  overall: score,
};
[@deriving (sexp, yojson)]
type section = {
  name: string,
  report,
};
[@deriving (sexp, yojson)]
type chapter = list(section);

module Main = {
  let settings = CoreSettings.on; /* Statics and Dynamics on */
  let name_to_exercise_export = path => {
    let all = path |> Yojson.Safe.from_file |> all_of_yojson;
    all.exercise |> Sexp.of_string |> Store.exercise_export_of_sexp;
  };

  /* ---- Code exercises ---- */

  let gen_code_grading_report = (exercise): report => {
    let zipper_pp = Printer.of_zipper;
    let terms =
      stitch_term(exercise.eds)
      |> map_stitched((_, {term, _}: TermItem.t) => term);
    let stitched_tests =
      map_stitched(
        (_, term) => {
          let evaluated =
            term
            |> CachedStatics.init_from_term(
                 ~settings,
                 ~is_dynamic_term=false,
               )
            |> ((x: CachedStatics.t) => x.elaborated)
            |> Evaluator.evaluate_and_limit(
                 ~step_limit=1000000,
                 ~env=Builtins.env_init,
               );
          switch (evaluated) {
          | StepLimitExceeded => None
          | Completed((_, evaluated)) =>
            evaluated
            |> EvaluatorState.get_tests
            |> TestResults.mk_results
            |> Option.some
          };
        },
        terms,
      );
    let grading_report = exercise.eds |> GradingReport.mk(~stitched_tests);
    let details = grading_report;
    let point_distribution = details.point_distribution;
    let test_validation = {
      max: point_distribution.test_validation,
      src: exercise.eds.your_tests.tests.state.zipper |> zipper_pp,
      percentage:
        details.test_validation_report |> TestValidationReport.percentage,
    };
    let mutation_testing = {
      max: point_distribution.mutation_testing,
      src: "",
      percentage:
        details.mutation_testing_report |> MutationTestingReport.percentage,
    };
    let impl_grading = {
      max: point_distribution.impl_grading,
      src: exercise.eds.your_impl.state.zipper |> zipper_pp,
      percentage:
        ImplGradingReport.percentage(
          details.impl_grading_report,
          details.syntax_report,
        ),
    };
    let overall = grading_report |> GradingReport.overall_score;
    let (a, b) = overall;
    let summary =
      Printf.sprintf("Overall: %.1f/%.1f\n\n", a, b)
      ++ item_to_summary("Test Validation", test_validation)
      ++ item_to_summary("Mutation Testing", mutation_testing)
      ++ item_to_summary("Impl Grading", impl_grading);
    {
      summary,
      overall,
    };
  };

  let grade_code_exercise = (spec, persistent_state): report => {
    let spec = unpersist(persistent_state, ~spec, ~instructor_mode=true);
    let zipper_spec: p(ZipperBase.t) =
      Web.CodeExercise.map(
        spec.eds,
        e => e.state.zipper,
        e => e.state.zipper,
      );
    {eds: zipper_spec |> eds_of_spec(~settings=CoreSettings.on)}
    |> gen_code_grading_report;
  };

  /* ---- Derivation exercises ---- */

  let grade_derivation_exercise =
      (spec: Web.DerivationExercise.spec, persistent_state): report => {
    let r = Web.GradeExercise.grade_derivation(spec, persistent_state);
    {
      summary: r.summary,
      overall: r.overall,
    };
  };

  /* ---- Theorem exercises (placeholder — reports max points) ---- */

  let grade_theorem_exercise =
      (spec: Web.TheoremExercise.spec, persistent_state): report => {
    let r = Web.GradeExercise.grade_theorem(spec, persistent_state);
    {
      summary: r.summary,
      overall: r.overall,
    };
  };

  /* ---- Dispatch ---- */

  let find_spec = (id: Id.t) =>
    ListUtil.findi_opt(
      spec => Id.equal(Web.Exercise.id_of(spec), id),
      specs,
    );

  let grade =
      (id: Uuidm.t, persistent: Model.persistent_exercise): option(section) => {
    switch (find_spec(id)) {
    | None => failwith("Invalid spec")
    | Some((_, spec)) =>
      let (name, report) =
        switch (spec, persistent) {
        | (Web.Exercise.Code(code_spec), Model.PCode(ps)) => (
            code_spec.title,
            grade_code_exercise(code_spec, ps),
          )
        | (Web.Exercise.Derivation(drv_spec), Model.PDerivation(ps)) => (
            drv_spec.title,
            grade_derivation_exercise(drv_spec, ps),
          )
        | (Web.Exercise.Theorem(thm_spec), Model.PTheorem(ps)) => (
            thm_spec.title,
            grade_theorem_exercise(thm_spec, ps),
          )
        /* Spec kind and persistent kind don't match; skip. */
        | _ => (
            "<mismatched persistent/spec>",
            {
              summary: "",
              overall: (0., 0.),
            },
          )
        };
      Some({
        name,
        report,
      });
    };
  };

  let run = () => {
    let hw_path = Sys.get_argv()[1];
    let output_path = Sys.get_argv()[2];
    let hw = name_to_exercise_export(hw_path);
    let export_chapter =
      hw.exercise_data
      |> List.filter_map(~f=((key, persistent_exercise)) =>
           grade(key, persistent_exercise)
         );
    export_chapter
    |> yojson_of_chapter
    |> Yojson.Safe.pretty_to_string
    |> Out_channel.output_string(Out_channel.create(output_path));
  };
};
Main.run();
