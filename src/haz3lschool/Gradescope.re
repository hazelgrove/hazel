open Haz3lcore;
// open Sexplib.Std;
open Haz3lschool;
open Core;

open Specs;
open GradePrelude.SchoolExercise;
open GradePrelude.Grading;


[@deriving (sexp, yojson)]
type item = percentage;
// type item = {
//   src: string,
//   percentage,
// };

[@deriving (sexp, yojson)]
type report = {
  point_distribution,
  test_validation: item,
  mutation_testing: item,
  impl_grading: item,
  overall: score,
};
[@deriving (sexp, yojson)]
type section = {
  idx: int,
  name: string,
  report,
};

[@deriving (sexp, yojson)]
type chapter = list(section);

module Main = {
  let name_to_school_export = path => {
    let yj = Yojson.Safe.from_file(path);
    switch (yj) {
    | `Assoc(l) =>
      let sch = List.Assoc.find_exn(~equal=String.(==), l, "school");
      switch (sch) {
      | `String(sch) =>
        let school_export = sch |> deserialize_school_export;
        school_export;
      | _ => failwith("School is not a string")
      };
    | _ => failwith("Json without school key")
    };
  };
  let gen_grading_report = exercise => {
    let model_results = ModelResults.init(spliced_elabs(exercise));
    let stitched_dynamics = stitch_dynamic(exercise, Some(model_results));
    let grading_report = exercise.eds |> GradingReport.mk(~stitched_dynamics);
    grading_report;
  };
  let gen_report = (grading_report: GradingReport.t) => {
    let details = grading_report;
    let point_distribution = details.point_distribution;
    let test_validation = details.test_validation_report |> TestValidationReport.percentage;
    let mutation_testing = details.mutation_testing_report |> MutationTestingReport.percentage;
    let impl_grading = details.impl_grading_report |> ImplGradingReport.percentage;
    let overall = grading_report |> GradingReport.overall_score;
    {
      point_distribution,
      test_validation,
      mutation_testing,
      impl_grading,
      overall,
    };
  };
  let run = () => {
    let hw_path = Sys.get_argv()[1];
    let hw = name_to_school_export(hw_path);
    let export_lst_pr =
      hw.exercise_data
      |> List.map(~f=(((name, idx) as key, persistent_state)) => {
           switch (find_key_opt(key, specs)) {
           | Some((_n, spec)) =>
             let exercise =
               unpersist_state(
                 persistent_state,
                 ~spec,
                 ~instructor_mode=true,
               );
             let report = exercise |> gen_grading_report |> gen_report;
             {idx, name, report};
           //  | None => (key |> yojson_of_key |> Yojson.Safe.to_string, "?")
           | None => failwith("Invalid spec")
           // List.nth(specs, 0)
           }
         });

    let s = export_lst_pr |> yojson_of_chapter |> Yojson.Safe.pretty_to_string;
    print_endline(s);
    // let yj_str_school_export =
    //   school_export |> yojson_of_school_export |> Yojson.Safe.to_string;
  };
};

Main.run();
