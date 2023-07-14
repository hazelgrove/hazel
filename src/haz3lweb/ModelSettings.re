open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  captions: bool,
  secondary_icons: bool,
  statics: bool,
  dynamics: bool,
  async_evaluation: bool,
  context_inspector: bool,
  instructor_mode: bool,
  benchmark: bool,
  mode: Editors.mode,
};

let default = {
  captions: true,
  secondary_icons: false,
  statics: true,
  dynamics: true,
  async_evaluation: false,
  context_inspector: false,
  instructor_mode: ExerciseSettings.show_instructor,
  benchmark: false,
  mode: Editors.Scratch,
};
let init_debug = {...default, mode: Editors.DebugLoad};

let key: string = "SETTINGS";
let serialize = s => s |> sexp_of_t |> Sexplib.Sexp.to_string;
let deserialize = s => {
  let settings = s |> Sexplib.Sexp.of_string |> t_of_sexp;
  if (settings.instructor_mode && !ExerciseSettings.show_instructor) {
    {...settings, instructor_mode: false};
  } else {
    settings;
  };
};
