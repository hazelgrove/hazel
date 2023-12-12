open Haz3lcore;
// open Sexplib.Std;
open Haz3lschool;
open Core;

open Specs;
open GradePrelude.Exercise;
open GradePrelude.Grading;

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
  let name_to_exercise_export = path => {
    let yj = Yojson.Safe.from_file(path);
    switch (yj) {
    | `Assoc(l) =>
      let sch = List.Assoc.find_exn(~equal=String.(==), l, "school");
      switch (sch) {
      | `String(sch) =>
        let exercise_export = sch |> deserialize_exercise_export;
        exercise_export;
      | _ => failwith("School is not a string")
      };
    | _ => failwith("Json without school key")
    };
  };
  let gen_grading_report = exercise => {
    let zipper_pp = zipper => {
      Printer.pretty_print(
        ~measured=Measured.of_segment(Zipper.seg_without_buffer(zipper)),
        zipper,
      );
    };
    let settings = CoreSettings.on;
    let model_results =
      ModelResults.init(~settings, spliced_elabs(settings, exercise));
    let stitched_dynamics =
      stitch_dynamic(settings, exercise, Some(model_results));
    let grading_report = exercise.eds |> GradingReport.mk(~stitched_dynamics);
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
    {summary, overall};
  };
  let run = () => {
    let hw_path = Sys.get_argv()[1];
    let hw = name_to_exercise_export(hw_path);
    let export_chapter =
      hw.exercise_data
      |> List.map(~f=(((name, _) as key, persistent_state)) => {
           switch (find_key_opt(key, specs)) {
           | Some((_n, spec)) =>
             let exercise =
               unpersist_state(
                 persistent_state,
                 ~spec,
                 ~instructor_mode=true,
               );
             let report = exercise |> gen_grading_report;
             {name, report};
           | None => failwith("Invalid spec")
           //  | None => (key |> yojson_of_key |> Yojson.Safe.to_string, "?")
           }
         });
    export_chapter
    |> yojson_of_chapter
    |> Yojson.Safe.pretty_to_string
    |> print_endline;
  };
};

Main.run();
