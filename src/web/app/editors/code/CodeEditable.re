open Haz3lcore;
type editor_id = string;
open Util;

/* A selectable editable code container component with statics and type-directed code completion. */
// This file follows conventions in [docs/ui-architecture.md]

module Model = CodeWithStatics.Model;

module Update = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Perform(Action.t)
    | DebugConsole(string);

  exception CantReset;

  let can_undo = (action: t) => {
    switch (action) {
    | Perform(action) => Action.is_historic(action)
    | DebugConsole(_) => false
    };
  };

  let update =
      (~globals: Globals.t, action: t, model: Model.t): Updated.t(Model.t) => {
    let perform = (action: Action.t, model: Model.t) =>
      Editor.Update.update(
        ~common=
          Common.{
            settings: globals.settings.core,
            font_metrics: globals.font_metrics,
            secondary_icons: globals.settings.secondary_icons,
            color_highlights: globals.color_highlights,
            statics: model.statics,
            dynamics: model.dynamics,
          },
        action,
        model.editor,
      )
      // |> (
      //   fun
      //   | Ok(editor) =>
      //     Model.{
      //       editor,
      //       statics: model.statics,
      //       dynamics: model.dynamics,
      //     }
      //   | Error(err) => raise(Action.Failure.Exception(err))
      // )
      |> (
        editor =>
          Model.{
            editor,
            statics: model.statics,
            dynamics: model.dynamics,
          }
          |> Updated.return(
               ~is_edit=Action.is_edit(action),
               ~recalculate=true,
               ~scroll_active=Action.should_scroll_active(action),
             )
      );
    switch (action) {
    | Perform(action) =>
      globals.settings.core.flip_animations && Action.should_animate(action)
        ? Animation.request([Animation.Actions.move("caret")]) : ();
      perform(action, model);
    | DebugConsole(key) =>
      DebugConsole.print(~settings=globals.settings, model, key);
      model |> Updated.return_quiet;
    };
  };

  let calculate = CodeWithStatics.Update.calculate;
};

module Focus = {
  // Editor selection is handled within Editor.t
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Editor.Focus.t;

  let get_cursor_info =
      (~globals: Globals.t, ~inject, ~read_only, model: Model.t, focus) =>
    Editor.Focus.get_cursor_info(
      ~common=
        Common.{
          settings: globals.settings.core,
          font_metrics: globals.font_metrics,
          secondary_icons: globals.settings.secondary_icons,
          color_highlights: globals.color_highlights,
          statics: model.statics,
          dynamics: model.dynamics,
        },
      ~inject=x => inject(Update.Perform(x)),
      ~read_only,
      model.editor,
      focus,
    );

  let jump_to_tile = (tile, model: Model.t) => {
    Editor.Update.jump_to_tile_action(tile, model.editor)
    |> Option.map(x => Update.Perform(x));
  };
};
