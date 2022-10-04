// open Haz3lcore;
open Haz3lschool;
open Core;

open GradePrelude.SchoolExercise;

// [@deriving (show({with_path: false}), sexp, yojson)]
[@deriving (sexp, yojson)]
type section = list((string, string));

// module type Spec = {
//   type t;
//   let exercise: spec
// };

module Main = {
  let get_school_export = yj => {
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
  let name_to_school_export = path => {
    let yj = Yojson.Safe.from_file(path);
    get_school_export(yj);
  };
  module Spec = Ex;
  let run = () => {
    // let spec_path = Sys.get_argv()[1];
    let spec = Ex.exercise;
    let hw_path = Sys.get_argv()[1];
    // let hw_path = Sys.get_argv()[2];
    let hw = name_to_school_export(hw_path);
    let export_lst_pr =
      hw.exercise_data
      |> List.map(~f=((_key, persistent_state)) => {
           print_endline(_key |> yojson_of_key |> Yojson.Safe.to_string);
           let eds =
             unpersist_state(persistent_state, ~spec, ~instructor_mode=true).
               eds;
           (
             eds.title,
             eds.point_distribution
             |> yojson_of_point_distribution
             |> Yojson.Safe.to_string,
           );
         });

    let s = export_lst_pr |> yojson_of_section |> Yojson.Safe.to_string;
    print_endline(s);
    // let yj_str_school_export =
    //   school_export |> yojson_of_school_export |> Yojson.Safe.to_string;
  };
};

Main.run();
