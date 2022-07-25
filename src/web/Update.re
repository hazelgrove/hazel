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
  | Simple(z) => LocalStorage.save_syntax(0, z)
  | Study(n, zs) =>
    assert(n < List.length(zs));
    LocalStorage.save_syntax(n, List.nth(zs, n));
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

let apply =
    (model: Model.t, update: t, _: State.t, ~schedule_action as _)
    : Result.t(Model.t) => {
  //print_endline("Update.apply");
  switch (update) {
  | Set(s_action) =>
    Ok({...model, settings: update_settings(s_action, model.settings)})
  | UpdateDoubleTap(double_tap) => Ok({...model, double_tap})
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
      history: ActionHistory.empty,
      id_gen,
      settings: LocalStorage.load_settings(),
      editor_model: Study(LocalStorage.load_editor_idx(), zs),
    });
  | LoadDefault =>
    let n = Model.current_editor(model);
    switch (LocalStorage.load_default_syntax(n, model.id_gen)) {
    | Some((z, id_gen)) =>
      Ok({
        ...model,
        history: ActionHistory.empty,
        editor_model: Model.put_zipper(model, Move.to_start(z)),
        id_gen,
      })
    | None => Error(FailedToLoad)
    };
  | Load =>
    let n = Model.current_editor(model);
    switch (LocalStorage.load_syntax(n, model.id_gen)) {
    | Some((z, id_gen)) =>
      Ok({
        ...model,
        history: ActionHistory.empty,
        editor_model: Model.put_zipper(model, Move.to_start(z)),
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
        assert(n < List.length(zs));
        LocalStorage.save_editor_idx(n);
        Ok({
          ...model,
          history: ActionHistory.empty,
          editor_model: Study(n, zs),
        });
      }
    }
  | SetShowBackpackTargets(b) =>
    Ok({
      ...model,
      history: ActionHistory.clear_just_failed(model.history),
      show_backpack_targets: b,
    })
  | SetFontMetrics(font_metrics) => Ok({...model, font_metrics})
  | SetLogoFontMetrics(logo_font_metrics) =>
    Ok({...model, logo_font_metrics})
  | PerformAction(a) =>
    let z_id = (Model.get_zipper(model), model.id_gen);
    switch (Perform.go(a, z_id)) {
    | Error(err) => Error(FailedToPerform(err))
    // TODO(andrew): refactor history
    | Ok((z, id_gen)) =>
      Ok({
        ...model,
        editor_model: Model.put_zipper(model, z),
        id_gen,
        history: ActionHistory.succeeded(a, z_id, model.history),
      })
    };
  | FailedInput(reason) => Error(UnrecognizedInput(reason))
  | Undo =>
    // TODO(andrew): refactor history
    let z_id = (Model.get_zipper(model), model.id_gen);
    switch (ActionHistory.undo(z_id, model.history)) {
    | None => Error(CantUndo)
    | Some(((z, id_gen), history)) =>
      Ok({
        ...model,
        editor_model: Model.put_zipper(model, z),
        id_gen,
        history,
      })
    };
  | Redo =>
    // TODO(andrew): refactor history
    let z_id = (Model.get_zipper(model), model.id_gen);
    switch (ActionHistory.redo(z_id, model.history)) {
    | None => Error(CantRedo)
    | Some(((z, id_gen), history)) =>
      Ok({
        ...model,
        editor_model: Model.put_zipper(model, z),
        id_gen,
        history,
      })
    };
  | MoveToNextHole(_d) =>
    // TODO restore
    Ok(model)
  };
};
