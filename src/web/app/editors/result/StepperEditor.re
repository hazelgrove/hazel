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
    refls: list(Id.t),
  };
};

module Update = {
  open Updated;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = CodeSelectable.Update.t;

  let update = (~globals, action, model: Model.t): Updated.t(Model.t) => {
    let* editor =
      CodeSelectable.Update.update(
        ~globals,
        ~dynamics=Language.Dynamics.Map.empty,
        action,
        model.editor,
      );
    Model.{
      editor,
      taken_steps: model.taken_steps,
      next_steps: model.next_steps,
      refls: model.refls,
    };
  };

  let can_undo = CodeSelectable.Update.can_undo;

  let calculate =
      (
        ~globals: Globals.t,
        ~stitch,
        ~dynamics: Language.Dynamics.Map.t,
        {editor, taken_steps, next_steps, refls}: Model.t,
      )
      : Model.t => {
    let editor =
      CodeSelectable.Update.calculate(
        ~common=Globals.to_common_global(globals),
        ~stitch,
        ~dynamics,
        ~is_dynamic_term=true,
        editor,
      );
    {
      editor,
      taken_steps,
      next_steps,
      refls,
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
    | TakeStep(int)
    | Refl(int);

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
          let editor = model.editor.editor;
          let globals =
            Common.t_of_global(
              ~statics=model.editor |> EditorManager.Model.get_statics,
              ~dynamics=Language.Dynamics.Map.empty,
              Globals.to_common_global(globals),
            );
        });
      overlays
      @ Deco.taken_steps(model.taken_steps)
      @ Deco.next_steps(model.next_steps, ~inject=x => signal(TakeStep(x)))
      @ Deco.refl_steps(model.refls, ~inject=x => signal(Refl(x)));
    };
    CodeSelectable.View.view(
      ~take_focus=f => signal(MakeActive(f)),
      ~focus=selected,
      ~common=
        Common.t_of_global(
          ~statics=model.editor |> EditorManager.Model.get_statics,
          ~dynamics=Language.Dynamics.Map.empty,
          Globals.to_common_global(globals),
        ),
      ~overlays,
      model.editor.editor,
    );
  };
};
