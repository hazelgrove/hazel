open Haz3lcore;
open Util_web;
open Core;
open Language;
open Web.CodeExercise;
open Web.CodeGrading;
open Web.Specs;
open Web.Export;
open Web.ExercisesMode;

/* Batch grading for student submissions, exposed via the Hazel CLI as the
   [grade-json] and [grade-report] subcommands. For each exercise in a
   submission, we dispatch on the exercise kind (Code / Derivation /
   Theorem) to produce a [section] summarising the score and details. */

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
          |> CachedStatics.init_from_term(~settings, ~is_dynamic_term=false)
          |> ((x: CachedStatics.t) => x.elaborated)
          |> Evaluator.evaluate_and_limit(
               ~step_limit=1000000,
               ~env=Builtins.env_init,
             );
        switch (evaluated) {
        | StepLimitExceeded => None
        | LimitedCompleted((_, evaluated)) =>
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
    Web.CodeExercise.map(spec.eds, e => e.state.zipper, e => e.state.zipper);
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
  ListUtil.findi_opt(spec => Id.equal(Web.Exercise.id_of(spec), id), specs);

let grade_one =
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

/* Grade a submission file and return the list of per-exercise sections. */
let grade_submission = (submission_path: string): chapter => {
  let hw = name_to_exercise_export(submission_path);
  hw.exercise_data
  |> List.filter_map(~f=((key, persistent_exercise)) =>
       grade_one(key, persistent_exercise)
     );
};

/* ---- Output: JSON ---- */

let to_json_string = (sections: chapter): string => {
  sections |> yojson_of_chapter |> Yojson.Safe.pretty_to_string;
};

/* ---- Output: human-readable text ----

     Ported from the render_report function in
     src/grading/grade/grade.py. Produces output like:

         Exercise Name  —  3.5/10.0
         ======================
         <summary...>

         ... more sections ...

         ----------------------------------------
         Total: 7.5/30.0
   */

/* Count Unicode code points in a UTF-8 encoded string. Used so that the
   [=] underline below each section header visually matches its width
   regardless of the em-dash and other multi-byte characters. */
let utf8_length = (s: string): int => {
  let len = ref(0);
  String.iter(
    ~f=
      c => {
        let b = Char.to_int(c);
        /* Count leading bytes in UTF-8: any byte not of the form 10xxxxxx. */
        if (b land 0xC0 != 0x80) {
          incr(len);
        };
      },
    s,
  );
  len^;
};

let render_report = (sections: chapter): string => {
  let buf = Buffer.create(1024);
  let total_earned = ref(0.0);
  let total_max = ref(0.0);

  List.iter(
    sections,
    ~f=section => {
      let (earned, maximum) = section.report.overall;
      total_earned := total_earned^ +. earned;
      total_max := total_max^ +. maximum;
      let header =
        Printf.sprintf("%s  —  %.1f/%.1f", section.name, earned, maximum);
      let underline = String.make(utf8_length(header), '=');
      Buffer.add_string(buf, header);
      Buffer.add_char(buf, '\n');
      Buffer.add_string(buf, underline);
      Buffer.add_char(buf, '\n');
      let summary = String.rstrip(section.report.summary);
      if (String.length(summary) > 0) {
        Buffer.add_string(buf, summary);
        Buffer.add_char(buf, '\n');
      };
      Buffer.add_char(buf, '\n');
    },
  );

  Buffer.add_string(buf, String.make(40, '-'));
  Buffer.add_char(buf, '\n');
  Buffer.add_string(
    buf,
    Printf.sprintf("Total: %.1f/%.1f", total_earned^, total_max^),
  );
  Buffer.add_char(buf, '\n');
  Buffer.contents(buf);
};

/* ---- IO helpers ---- */

let write_to_output = (~output: option(string), contents: string): unit => {
  switch (output) {
  | None => print_string(contents)
  | Some(path) =>
    let oc = Out_channel.create(path);
    Out_channel.output_string(oc, contents);
    Out_channel.close(oc);
  };
};

/* ---- CLI entry points ---- */

let grade_json = (submission_path: string, output: option(string)): unit => {
  let sections = grade_submission(submission_path);
  write_to_output(~output, to_json_string(sections) ++ "\n");
};

let grade_report = (submission_path: string, output: option(string)): unit => {
  let sections = grade_submission(submission_path);
  write_to_output(~output, render_report(sections));
};
