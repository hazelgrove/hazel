open Sexplib.Std;

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
  let settings = Store.Settings.export();
  let explainThisModel = Store.ExplainThisModel.export();
  let settings_obj = Store.Settings.load();
  let scratch = Store.Scratch.export(~settings=settings_obj.core.evaluation);
  let documentation =
    Store.Documentation.export(~settings=settings_obj.core.evaluation);
  let exercise =
    Store.Exercise.export(
      ~specs=ExerciseSettings.exercises,
      ~instructor_mode,
    );
  {settings, explainThisModel, scratch, documentation, exercise, log};
};

let export_all = (~instructor_mode, ~log) => {
  mk_all(~instructor_mode, ~log) |> yojson_of_all;
};

let import_all = (data, ~specs) => {
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
  let settings = Store.Settings.import(all.settings);
  Store.ExplainThisModel.import(all.explainThisModel);
  let instructor_mode = settings.instructor_mode;
  let editing_title = settings.editing_title;
  Store.Scratch.import(~settings=settings.core.evaluation, all.scratch);
  Store.Exercise.import(
    all.exercise,
    ~specs,
    ~instructor_mode,
    ~editing_title,
  );
  Log.import(all.log);
};
