open Util;
open Haz3lcore;

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

  let update = (~settings, action, model: Model.t): Updated.t(Model.t) => {
    let* editor =
      CodeSelectable.Update.update(~settings, action, model.editor);
    Model.{
      editor,
      taken_steps: model.taken_steps,
      next_steps: model.next_steps,
    };
  };

  let calculate =
      (
        ~settings,
        ~is_edited,
        ~stitch,
        {editor, taken_steps, next_steps}: Model.t,
      )
      : Model.t => {
    let editor =
      CodeSelectable.Update.calculate(~settings, ~is_edited, ~stitch, editor);
    {editor, taken_steps, next_steps};
  };
};

module Selection = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = CodeSelectable.Selection.t;
};

module View = {
  type event =
    | MakeActive
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
          let editor = model.editor.editor;
          let globals = globals;
          let statics = model.editor.statics;
        });
      overlays
      @ Deco.taken_steps(model.taken_steps)
      @ Deco.next_steps(model.next_steps, ~inject=x => signal(TakeStep(x)));
    };
    CodeSelectable.View.view(
      ~signal=
        fun
        | MakeActive => signal(MakeActive),
      ~selected,
      ~globals,
      ~overlays,
      model.editor,
    );
  };
};
