open Haz3lcore;

module Stepped = {
  type model = {
    editor: CodeWithStatics.Model.t,
    step_id: option(Id.t),
  };

  type event = CodeWithStatics.View.event;

  let view = (~globals: Globals.t, ~overlays=[], model: model) => {
    let overlays = {
      module Deco =
        Deco.Deco({
          let editor = model.editor.editor;
          let globals = globals;
        });
      overlays @ Deco.taken_step(model.step_id);
    };
    CodeWithStatics.View.view(~globals, ~overlays, model.editor);
  };
};

module Steppable = {
  type model = {
    editor: CodeWithStatics.Model.t,
    next_steps: list(Id.t),
  };

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
    CodeWithStatics.View.view(~globals, ~overlays, model.editor);
  };
};
