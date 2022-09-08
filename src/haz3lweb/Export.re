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

let of_simple = (filename: string, simple: Editors.simple): t =>
  simple
  |> LocalStorage.prep_simple_in
  |> LocalStorage.yojson_of_simple_without_history
  |> of_(filename);

let of_study = (filename: string, study: Editors.study): t =>
  study
  |> LocalStorage.prep_study_in
  |> LocalStorage.yojson_of_study_without_history
  |> of_(filename);

let of_school = (filename: string, school: Editors.school): t =>
  school
  |> LocalStorage.prep_school_in
  |> LocalStorage.yojson_of_school_without_history
  |> of_(filename);

let download = ({contents, filename}: t): unit =>
  JsUtil.download_string_file(filename, "application/json", contents);
