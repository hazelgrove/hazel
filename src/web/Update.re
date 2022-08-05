open Sexplib.Std;
open Util;
open Core;

[@deriving (show({with_path: false}), sexp, yojson)]
type settings_action =
  | Captions
  | WhitespaceIcons;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Set(settings_action)
  | UpdateDoubleTap(option(float))
  | LoadInit
  | LoadDefault
  | Load
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
    | Captions => {...settings, captions: !settings.captions}
    | WhitespaceIcons => {
        ...settings,
        whitespace_icons: !settings.whitespace_icons,
      }
    };
  LocalStorage.save_settings(settings);
  settings;
};

let load_editor = (model: Model.t) =>
  switch (model.editor_model) {
  | Simple(_) =>
    let (id_gen, editor) = LocalStorage.load_simple();
    {...model, id_gen, editor_model: Simple(editor)};
  | Study(_) =>
    let (id_gen, idx, editors) = LocalStorage.load_study();
    {...model, id_gen, editor_model: Study(idx, editors)};
  | School(_) =>
    let (id_gen, idx, editors) = LocalStorage.load_school();
    {...model, id_gen, editor_model: School(idx, editors)};
  };

let apply =
    (model: Model.t, update: t, _: State.t, ~schedule_action as _)
    : Result.t(Model.t) => {
  switch (update) {
  | Set(s_action) =>
    Ok({...model, settings: update_settings(s_action, model.settings)})
  | UpdateDoubleTap(double_tap) => Ok({...model, double_tap})
  | LoadInit =>
    let model = load_editor(model);
    Ok({...model, settings: LocalStorage.load_settings()});
  /*
   | LoadInit =>
     let (zs, id_gen) =
       List.fold_left(
         ((z_acc, id_gen: IdGen.state), n) =>
           switch (LocalStorage.load_syntax(n, id_gen)) {
           | Some((z, id_gen)) => (z_acc @ [z], id_gen)
           | None => (z_acc @ [Model.empty_zipper], id_gen)
           },
         ([], model.id_gen),
         List.init(LocalStorage.num_editors, n => n),
       );
     let zs = List.map(Move.to_start, zs);
     Ok({
       ...model,
       id_gen,
       settings: LocalStorage.load_settings(),
       editor_model: Study(LocalStorage.load_editor_idx(), zs),
     });*/
  | LoadDefault =>
    switch (model.editor_model) {
    | Simple(_) =>
      let (id_gen, editor) = Model.simple_init;
      Ok({...model, editor_model: Simple(editor), id_gen});
    | Study(_) =>
      let (id_gen, idx, editors) = Model.study_init;
      Ok({...model, editor_model: Study(idx, editors), id_gen});
    | School(_) =>
      let (id_gen, idx, editors) = Model.school_init;
      Ok({...model, editor_model: School(idx, editors), id_gen});
    }
  | Load =>
    //TODO(andrew): update or remove this
    let n = Model.current_editor(model);
    switch (LocalStorage.load_syntax(n, model.id_gen)) {
    | Some((z, id_gen)) =>
      Ok({
        ...model,
        editor_model:
          Model.put_editor(
            model,
            {zipper: Move.to_start(z), history: ActionHistory.empty},
          ),
        id_gen,
      })
    | None => Error(FailedToLoad)
    };
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
        LocalStorage.save_editor_idx(n);
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
    //TODO(andrew): more careful id_gen?
    //note: empty values will be filled in by load
    let model =
      switch (model.editor_model) {
      | Simple(_) => {...model, editor_model: Study(0, [])}
      | Study(_) => {...model, editor_model: School(0, [])}
      | School(_) => {...model, editor_model: Simple(Model.mk_editor(0))}
      };
    Ok(load_editor(model));
  | SetShowBackpackTargets(b) =>
    Ok({
      ...model,
      //history: ActionHistory.clear_just_failed(model.history),
      show_backpack_targets: b,
    })
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
