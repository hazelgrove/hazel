open Util;
open Haz3lcore;

/* This file follows conventions in [docs/ui-architecture.md] */

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    // Updated
    editor: CodeSelectable.Model.t,
    // Read-only
    taken_steps: list(Id.t),
    next_steps: list(Id.t),
  };
};

module Update = {
  open Updated;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = CodeSelectable.Update.t;

  let update = (~globals, action, model: Model.t): Updated.t(Model.t) => {
    let* editor =
      CodeSelectable.Update.update(~globals, action, model.editor);
    Model.{
      editor,
      taken_steps: model.taken_steps,
      next_steps: model.next_steps,
    };
  };

  let can_undo = CodeSelectable.Update.can_undo;

  let calculate =
      (
        ~globals,
        ~stitch,
        ~dynamics: Language.Dynamics.Map.t,
        {editor, taken_steps, next_steps}: Model.t,
      )
      : Model.t => {
    let editor =
      CodeSelectable.Update.calculate(
        ~globals,
        ~stitch,
        ~dynamics,
        ~is_dynamic_term=true,
        editor,
      );
    {
      editor,
      taken_steps,
      next_steps,
    };
  };
};

module Selection = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = CodeSelectable.Selection.t;

  let get_cursor_info = CodeSelectable.Selection.get_cursor_info;
};

module View = {
  type event =
    | MakeActive(Editor.Focus.t)
    | TakeStep(int);

  let view =
      (
        ~globals: Globals.t,
        ~signal: event => 'a,
        ~overlays=[],
        ~selected,
        model: Model.t,
      ) => {
    let overlays = {
      module Deco =
        Deco.Deco({
          type projector = Projector.Model.t;
          type projector_kind = ProjectorKind.t;
          type projector_action = Projector.Update.t;
          let editor = model.editor.editor;
          let globals =
            Common.{
              settings: globals.settings.core,
              font_metrics: globals.font_metrics,
              secondary_icons: globals.settings.secondary_icons,
              show_backpack_targets: globals.show_backpack_targets,
              color_highlights: globals.color_highlights,
              statics: model.editor.statics,
              dynamics: model.editor.dynamics,
            };
        });
      overlays
      @ Deco.taken_steps(model.taken_steps)
      @ Deco.next_steps(model.next_steps, ~inject=x => signal(TakeStep(x)));
    };
    CodeSelectable.View.view(
      ~take_focus=f => signal(MakeActive(f)),
      ~focus=selected,
      ~common={
        settings: globals.settings.core,
        font_metrics: globals.font_metrics,
        secondary_icons: globals.settings.secondary_icons,
        show_backpack_targets: globals.show_backpack_targets,
        color_highlights: globals.color_highlights,
        statics: model.editor.statics,
        dynamics: model.editor.dynamics,
      },
      ~overlays,
      model.editor.editor,
    );
  };
};
