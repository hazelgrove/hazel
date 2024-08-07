open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type all = {
  settings: string,
  explainThisModel: string,
  scratch: string,
  exercise: string,
  documentation: string,
  log: string,
};

// fallback for saved state prior to release of lang doc in 490F22
[@deriving (show({with_path: false}), sexp, yojson)]
type all_f22 = {
  settings: string,
  scratch: string,
  exercise: string,
  log: string,
};

let mk_all = (~instructor_mode, ~log) => {
  let settings = Settings.Store.export();
  let explainThisModel = ExplainThisModel.Store.export();
  let scratch = ScratchMode.Store.export();
  let documentation = ScratchMode.StoreDocumentation.export();
  let exercise = ExercisesMode.Store.export(~instructor_mode);
  {settings, explainThisModel, scratch, documentation, exercise, log};
};

let export_all = (~instructor_mode, ~log) => {
  mk_all(~instructor_mode, ~log) |> yojson_of_all;
};

let import_all = (~import_log: string => unit, data, ~specs) => {
  let all =
    try(data |> Yojson.Safe.from_string |> all_of_yojson) {
    | _ =>
      let all_f22 = data |> Yojson.Safe.from_string |> all_f22_of_yojson;
      {
        settings: all_f22.settings,
        scratch: all_f22.scratch,
        documentation: "",
        exercise: all_f22.exercise,
        log: all_f22.log,
        explainThisModel: "",
      };
    };
  Settings.Store.import(all.settings);
  let settings = Settings.Store.load();
  ExplainThisModel.Store.import(all.explainThisModel);
  let instructor_mode = settings.instructor_mode;
  ScratchMode.Store.import(all.scratch);
  ExercisesMode.Store.import(all.exercise, ~specs, ~instructor_mode);
  import_log(all.log);
};