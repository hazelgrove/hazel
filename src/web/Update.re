open Sexplib.Std;
open Util;
open Core;

[@deriving (show({with_path: false}), sexp, yojson)]
type settings_action =
  | Captions
  | WhitespaceIcons
  | Statics
  | Dynamics
  | Student
  | Mode(Model.mode);

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Set(settings_action)
  | UpdateDoubleTap(option(float))
  | LoadInit
  | LoadDefault
  | Save
  | ToggleMode
  | SwitchEditor(int)
  | SetFontMetrics(FontMetrics.t)
  | SetLogoFontMetrics(FontMetrics.t)
  | PerformAction(Perform.Action.t)
  | FailedInput(FailedInput.reason) //TODO(andrew): refactor as failure?
  | Undo
  | Redo
  | SetShowBackpackTargets(bool)
  | MoveToNextHole(Direction.t);

module Failure = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | CantUndo
    | CantRedo
    | FailedToLoad
    | FailedToSwitch
    | UnrecognizedInput(FailedInput.reason)
    | FailedToPerform(Perform.Action.Failure.t)
    | Exception(string);
};

module Result = {
  include Result;
  type t('success) = Result.t('success, Failure.t);
};

let save = (model: Model.t): unit =>
  switch (model.editor_model) {
  | Simple(ed) => LocalStorage.save_simple((model.id_gen, ed))
  | Study(n, eds) => LocalStorage.save_study((model.id_gen, n, eds))
  | School(n, eds) =>
    assert(n < List.length(eds));
    LocalStorage.save_school((model.id_gen, n, eds));
  };

let update_settings =
    (a: settings_action, settings: Model.settings): Model.settings => {
  let settings =
    switch (a) {
    | Statics =>
      /* NOTE: dynamics depends on statics, so if dynamics is on and
         we're turning statics off, turn dynamics off as well */
      {
        ...settings,
        statics: !settings.statics,
        dynamics: !settings.statics && settings.dynamics,
      }
    | Dynamics => {...settings, dynamics: !settings.dynamics}
    | Captions => {...settings, captions: !settings.captions}
    | WhitespaceIcons => {
        ...settings,
        whitespace_icons: !settings.whitespace_icons,
      }
    | Student => {...settings, student: !settings.student}
    | Mode(mode) => {...settings, mode}
    };
  LocalStorage.save_settings(settings);
  settings;
};

let load_editor = (model: Model.t): Model.t =>
  switch (model.settings.mode) {
  | Simple =>
    let (id_gen, editor) = LocalStorage.load_simple();
    {...model, id_gen, editor_model: Simple(editor)};
  | Study =>
    let (id_gen, idx, editors) = LocalStorage.load_study();
    {...model, id_gen, editor_model: Study(idx, editors)};
  | School =>
    let (id_gen, idx, editors) = LocalStorage.load_school();
    {...model, id_gen, editor_model: School(idx, editors)};
  };

let load_default_editor = (model: Model.t): Model.t =>
  switch (model.settings.mode) {
  | Simple =>
    let (id_gen, editor) = Model.simple_init;
    {...model, editor_model: Simple(editor), id_gen};
  | Study =>
    let (id_gen, idx, editors) = Study.init;
    {...model, editor_model: Study(idx, editors), id_gen};
  | School =>
    let (id_gen, idx, editors) = School.init;
    {...model, editor_model: School(idx, editors), id_gen};
  };

let rotate_mode = (mode: Model.mode): Model.mode =>
  switch (mode) {
  | Simple => Study
  | Study => School
  | School => Simple
  };

let apply =
    (model: Model.t, update: t, _: State.t, ~schedule_action as _)
    : Result.t(Model.t) => {
  switch (update) {
  | Set(s_action) =>
    Ok({...model, settings: update_settings(s_action, model.settings)})
  | UpdateDoubleTap(double_tap) => Ok({...model, double_tap})
  | LoadInit =>
    // NOTE: load settings first to get last editor mode
    let model = {...model, settings: LocalStorage.load_settings()};
    Ok(load_editor(model));
  | LoadDefault => Ok(load_default_editor(model))
  | Save =>
    save(model);
    Ok(model);
  | SwitchEditor(n) =>
    switch (model.editor_model) {
    | Simple(_) => Error(FailedToSwitch)
    | Study(m, _) when m == n => Error(FailedToSwitch)
    | Study(_, zs) =>
      switch (n < List.length(zs)) {
      | false => Error(FailedToSwitch)
      | true =>
        LocalStorage.save_study((model.id_gen, n, zs));
        Ok({...model, editor_model: Study(n, zs)});
      }
    | School(m, _) when m == n => Error(FailedToSwitch)
    | School(_, zs) =>
      switch (n < List.length(zs)) {
      | false => Error(FailedToSwitch)
      | true =>
        LocalStorage.save_school((model.id_gen, n, zs));
        Ok({...model, editor_model: School(n, zs)});
      }
    }
  | ToggleMode =>
    let model = {
      ...model,
      settings:
        update_settings(
          Mode(rotate_mode(model.settings.mode)),
          model.settings,
        ),
    };
    Ok(load_editor(model));
  | SetShowBackpackTargets(b) => Ok({...model, show_backpack_targets: b})
  | SetFontMetrics(font_metrics) => Ok({...model, font_metrics})
  | SetLogoFontMetrics(logo_font_metrics) =>
    Ok({...model, logo_font_metrics})
  | PerformAction(a) =>
    let Model.{zipper, history} = Model.get_editor(model);
    let z_id = (zipper, model.id_gen);
    switch (Perform.go(a, z_id)) {
    | Error(err) => Error(FailedToPerform(err))
    | Ok((zipper, id_gen)) =>
      let history = ActionHistory.succeeded(a, z_id, history);
      Ok({
        ...model,
        id_gen,
        editor_model: Model.put_editor(model, {zipper, history}),
      });
    };
  | FailedInput(reason) => Error(UnrecognizedInput(reason))
  | Undo =>
    let Model.{zipper, history} = Model.get_editor(model);
    let z_id = (zipper, model.id_gen);
    switch (ActionHistory.undo(z_id, history)) {
    | None => Error(CantUndo)
    | Some(((zipper, id_gen), history)) =>
      Ok({
        ...model,
        id_gen,
        editor_model: Model.put_editor(model, {zipper, history}),
      })
    };
  | Redo =>
    let Model.{zipper, history} = Model.get_editor(model);
    let z_id = (zipper, model.id_gen);
    switch (ActionHistory.redo(z_id, history)) {
    | None => Error(CantRedo)
    | Some(((zipper, id_gen), history)) =>
      Ok({
        ...model,
        id_gen,
        editor_model: Model.put_editor(model, {zipper, history}),
      })
    };
  | MoveToNextHole(_d) =>
    // TODO restore
    Ok(model)
  };
};
