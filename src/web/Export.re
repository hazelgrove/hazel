open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type full_state = {
  settings: string,
  explainThisModel: string,
  mode: string,
  scratch: string,
  tutorial: string,
  exercise: string,
  documentation: string,
};

let mk_full_state = (~core_settings, ~instructor_mode) => {
  let settings = Settings.Store.export();
  let explainThisModel = ExplainThisModel.Store.export();
  let mode = Editors.StoreMode.export();
  let (scratch_current, scratch_slides) = Init.startup.scratch;
  let scratch =
    ScratchMode.Persist.export_all(
      "scratch",
      ~default_names=List.map(fst, scratch_slides),
      ~default_current=scratch_current,
    );
  let (doc_current, doc_slides) = Init.startup.documentation;
  let documentation =
    ScratchMode.Persist.export_all(
      "doc",
      ~default_names=List.map(fst, doc_slides),
      ~default_current=doc_current,
    );
  let tutorial =
    TutorialsMode.Store.export(~settings=core_settings, ~instructor_mode);
  let exercise =
    ExercisesMode.Store.export(~settings=core_settings, ~instructor_mode);
  {
    settings,
    explainThisModel,
    mode,
    scratch,
    documentation,
    exercise,
    tutorial,
  };
};

[@deriving (show({with_path: false}), sexp, yojson)]
type log_export = {
  initial_state: option(full_state),
  log: string,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type submission = {
  initial_state: option(full_state),
  final_state: full_state,
  log: string,
};

let export_submission =
    (~settings, ~instructor_mode, ~initial_state=None, ~log, ()) =>
  {
    initial_state,
    final_state: mk_full_state(~core_settings=settings, ~instructor_mode),
    log,
  }
  |> yojson_of_submission;

let import_full_state = (state: full_state, ~exercise_specs, ~tutorial_specs) => {
  Settings.Store.import(state.settings);
  let settings = Settings.Store.load();
  ExplainThisModel.Store.import(state.explainThisModel);
  Editors.StoreMode.import(state.mode);
  let instructor_mode = settings.instructor_mode;
  ScratchMode.Persist.import_all("scratch", state.scratch);
  if (state.documentation != "") {
    ScratchMode.Persist.import_all("doc", state.documentation);
  };
  ExercisesMode.Store.import(
    ~settings,
    state.exercise,
    ~exercise_specs,
    ~instructor_mode,
  );
  TutorialsMode.Store.import(
    ~settings=settings.core,
    state.tutorial,
    ~tutorial_specs,
    ~instructor_mode,
  );
};
