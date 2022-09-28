open Haz3lcore;
open Haz3lschooldata;
open Core;

include SchoolData.SchoolExercise({
  type node = unit;
  let default = ();
});

[@deriving (sexp, yojson)]
// [@deriving (show({with_path: false}), sexp, yojson)]
type section = list((string, string));

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
  let run = () => {
    let file_path = Sys.get_argv()[1];
    let yj = Yojson.Safe.from_file(file_path);
    let school_export = get_school_export(yj);
    let export_lst_pr =
      school_export.exercise_data
      |> List.map(~f=((_key, (_, _, pos_zippers))) => {
           pos_zippers
           |> List.map(~f=((pos, zipper)) =>
                (
                  sexp_of_pos(pos) |> Sexp.to_string_hum,
                  Printer.to_string_basic(zipper),
                )
              )
         })
      |> List.concat;
    let s = export_lst_pr |> yojson_of_section |> Yojson.Safe.to_string;
    print_endline(s);
    // let yj_str_school_export =
    //   school_export |> yojson_of_school_export |> Yojson.Safe.to_string;
  };
};

Main.run();
