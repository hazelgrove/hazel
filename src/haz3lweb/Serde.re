open Haz3lschooldata;
open Core;

include SchoolData.State({
  type node = unit;
});

module Main = {
  let run = () => {
    let file_path = Sys.get_argv()[1];
    let yj = Yojson.Safe.from_file(file_path);
    switch (yj) {
    | `Assoc(l) =>
      let sch = List.Assoc.find_exn(~equal=String.(==), l, "school");
      switch (sch) {
      | `String(sch) =>
        let school_export =
          sch |> Sexplib.Sexp.of_string |> school_export_of_sexp;
        let yj_school_export =
          school_export |> yojson_of_school_export |> Yojson.Safe.to_string;
        print_endline(yj_school_export);
        ();
      | _ => failwith("School is not a string")
      };
    | _ => failwith("Json without school key")
    };
    ();
  };
};

Main.run();
