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
  scratch: LocalStorage.scratch_without_history,
  school: LocalStorage.school_without_history,
  settings: Model.settings,
  log: string,
};

let all = (filename: string) => {
  let settings = LocalStorage.load_settings();
  let data: all = {
    scratch: LocalStorage.load_scratch_without_history(),
    school:
      LocalStorage.load_school_without_history(
        ~instructor_mode=settings.instructor_mode,
      ),
    settings,
    log: Log.get_json_update_log_string(),
  };
  yojson_of_all(data) |> of_(filename);
};

let download = ({contents, filename}: t): unit =>
  JsUtil.download_string_file(filename, "application/json", contents);
