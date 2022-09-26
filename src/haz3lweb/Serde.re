open Haz3lcore;
open Haz3lschooldata;
open Core;

include SchoolData.State({
  type node = unit;
});

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
    let _ =
      school_export.exercise_data
      |> List.map(~f=((_key, (_, _, pos_zippers))) => {
           print_endline("???");
           pos_zippers
           |> List.map(~f=(_pos, zipper) => {
                print_endline(Printer.to_string_basic(zipper))
              });
         });
    // let yj_str_school_export =
    //   school_export |> yojson_of_school_export |> Yojson.Safe.to_string;
    ();
  };
};

Main.run();
