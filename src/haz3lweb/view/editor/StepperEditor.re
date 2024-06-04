open Haz3lcore;

module Stepped = {
  type model = {
    editor: ReadOnlyEditor.Model.t,
    step_id: option(Id.t),
  };

  type action = ReadOnlyEditor.Update.t;

  type event = ReadOnlyEditor.View.event;

  let view = (~globals: Globals.t, ~overlays=[], model: model) => {
    let overlays = {
      module Deco =
        Deco.Deco({
          let editor = model.editor.editor;
          let globals = globals;
        });
      overlays @ Deco.taken_step(model.step_id);
    };
    ReadOnlyEditor.View.view(~globals, ~overlays, model.editor);
  };
};

module Steppable = {
  type model = {
    editor: CodeEditor.Model.t,
    next_steps: list(Id.t),
  };

  type action = CodeEditor.Update.t;

  type event =
    | TakeStep(int);

  let view =
      (
        ~globals: Globals.t,
        ~signal: event => Ui_effect.t(unit),
        ~overlays=[],
        model: model,
      ) => {
    let overlays = {
      module Deco =
        Deco.Deco({
          let editor = model.editor.editor;
          let globals = globals;
        });
      overlays
      @ Deco.next_steps(model.next_steps, ~inject=x => signal(TakeStep(x)));
    };
    ReadOnlyEditor.View.view(~globals, ~overlays, model.editor);
  };
};
