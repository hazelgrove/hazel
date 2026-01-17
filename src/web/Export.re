open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type all = {
  settings: string,
  explainThisModel: string,
  scratch: string,
  tutorial: string,
  exercise: string,
  documentation: string,
  log: string,
};

// fallback for saved state prior to release of lang doc in 490F22
[@deriving (show({with_path: false}), sexp, yojson)]
type all_public = {
  settings: string,
  scratch: string,
  exercise: string,
  tutorial: string,
  log: string,
};

let mk_all = (~core_settings, ~instructor_mode, ~log) => {
  let settings = Settings.Store.export();
  let explainThisModel = ExplainThisModel.Store.export();
  let scratch = ScratchMode.Store.export();
  let documentation = ScratchMode.StoreDocumentation.export();
  let tutorial =
    TutorialsMode.Store.export(~settings=core_settings, ~instructor_mode);
  let exercise =
    ExercisesMode.Store.export(~settings=core_settings, ~instructor_mode);
  {
    settings,
    explainThisModel,
    scratch,
    documentation,
    exercise,
    tutorial,
    log,
  };
};

let export_all = (~settings, ~instructor_mode, ~log) => {
  mk_all(~core_settings=settings, ~instructor_mode, ~log) |> yojson_of_all;
};

let import_all =
    (~import_log: string => unit, data, ~exercise_specs, ~tutorial_specs) => {
  let all =
    try(data |> Yojson.Safe.from_string |> all_of_yojson) {
    | _ =>
      let all_public = data |> Yojson.Safe.from_string |> all_public_of_yojson;
      {
        settings: all_public.settings,
        scratch: all_public.scratch,
        documentation: "",
        exercise: all_public.exercise,
        tutorial: all_public.tutorial,
        log: all_public.log,
        explainThisModel: "",
      };
    };
  Settings.Store.import(all.settings);
  let settings = Settings.Store.load();
  ExplainThisModel.Store.import(all.explainThisModel);
  let instructor_mode = settings.instructor_mode;
  ScratchMode.Store.import(all.scratch);
  ExercisesMode.Store.import(
    ~settings,
    all.exercise,
    ~exercise_specs,
    ~instructor_mode,
  );
  TutorialsMode.Store.import(
    ~settings=settings.core,
    all.tutorial,
    ~tutorial_specs,
    ~instructor_mode,
  );
  import_log(all.log);
};
