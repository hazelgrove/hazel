open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type all = {
  settings: string,
  scratch: string,
  school: string,
  log: string,
};

let all = (~instructor_mode) => {
  let settings = LocalStorage.Settings.export();
  let specs = School.exercises;
  {
    settings,
    scratch: LocalStorage.Scratch.export(),
    school: LocalStorage.School.export(~specs, ~instructor_mode),
    log: Log.export(),
  };
};

let export_all = (~instructor_mode) => {
  all(~instructor_mode) |> yojson_of_all;
};

let download_json = (filename, contents): unit =>
  JsUtil.download_string_file(
    filename ++ ".json",
    "application/json",
    contents |> Yojson.Safe.to_string,
  );

let import = (data, ~specs) => {
  let all = data |> Yojson.Safe.from_string |> all_of_yojson;
  let settings = LocalStorage.Settings.import(all.settings); // TODO how does it get into model?
  print_endline("settings imported");
  let instructor_mode = settings.instructor_mode;
  LocalStorage.Scratch.import(all.scratch);
  print_endline("scratch imported");
  LocalStorage.School.import(all.school, ~specs, ~instructor_mode);
  print_endline("school imported");
  Log.import(all.log);
  print_endline("log imported");
};
