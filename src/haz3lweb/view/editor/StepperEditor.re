open Haz3lcore;

module Stepped = {
  type model = {
    editor: ReadOnlyEditor.model,
    step: option(EvaluatorStep.step),
    step_id: option(Id.t),
  };

  type action = ReadOnlyEditor.action;

  type event = ReadOnlyEditor.event;

  type selection = ReadOnlyEditor.selection;

  let view = (~globals: Globals.t, ~overlays=[], model: model) => {
    let overlays = {
      module Deco =
        Deco.Deco({
          let editor = model.editor.editor;
          let globals = globals;
        });
      overlays @ Deco.taken_step(model.step_id);
    };
    ReadOnlyEditor.view(~globals, ~overlays, model.editor);
  };
};

module Steppable = {
  type model = {
    editor: CodeEditor.model,
    next_steps: list(Id.t),
  };

  type action = CodeEditor.action;

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
    ReadOnlyEditor.view(~globals, ~overlays, model.editor);
  };
};
