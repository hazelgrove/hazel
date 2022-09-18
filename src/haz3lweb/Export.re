open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  contents: string,
  filename: string,
};

let of_ = (filename: string, contents: Yojson.Safe.t): t => {
  let contents = contents |> Yojson.Safe.to_string;
  let filename = filename ++ ".json";
  {contents, filename};
};

[@deriving (show({with_path: false}), sexp, yojson)]
type all = {
  scratch: string,
  school: string,
  settings: string,
  log: string,
};

let all =
    (filename: string, ~specs: list(SchoolExercise.spec), ~instructor_mode) => {
  let data: all = {
    scratch:
      Option.get(LocalStorage.get_localstore(LocalStorage.save_scratch_key)),
    school: LocalStorage.export_school(~specs, ~instructor_mode),
    settings:
      Option.get(
        LocalStorage.get_localstore(LocalStorage.save_settings_key),
      ),
    log: Log.get_json_update_log_string(),
  };
  yojson_of_all(data) |> of_(filename);
};

let download = ({contents, filename}: t): unit =>
  JsUtil.download_string_file(filename, "application/json", contents);
