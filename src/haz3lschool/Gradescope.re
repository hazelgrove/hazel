// open Haz3lcore;
open Haz3lschool;
open Core;

open GradePrelude.SchoolExercise;
open Specs;

[@deriving (sexp, yojson)]
type section = list((string, string));

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
  let run = () => {
    let hw_path = Sys.get_argv()[1];
    let hw = name_to_school_export(hw_path);
    let export_lst_pr =
      hw.exercise_data
      |> List.map(~f=((key, persistent_state)) => {
           switch (find_key_opt(key, specs)) {
           | Some((_n, spec)) =>
             let state =
               unpersist_state(
                 persistent_state,
                 ~spec,
                 ~instructor_mode=true,
               );
             (
               state.eds.title,
               state.eds.point_distribution
               |> yojson_of_point_distribution
               |> Yojson.Safe.to_string,
             );
           | None => (key |> yojson_of_key |> Yojson.Safe.to_string, "?")
           //  | None => failwith("Invalid spec")
           // List.nth(specs, 0)
           }
         });

    let s = export_lst_pr |> yojson_of_section |> Yojson.Safe.to_string;
    print_endline(s);
    // let yj_str_school_export =
    //   school_export |> yojson_of_school_export |> Yojson.Safe.to_string;
  };
};

Main.run();
