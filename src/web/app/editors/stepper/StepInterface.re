open Util;
open Language;

module type STEP = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type focus;

  let update: (~settings: Settings.t, action, model) => Updated.t(model);

  let can_undo: action => bool;

  let calculate:
    (
      ~settings: Calc.t(CoreSettings.t),
      ~hidden: Calc.saved(bool),
      ~exp: Calc.t(Exp.t),
      ~ctx: Calc.t(Ctx.t),
      ~env: Calc.t(ClosureEnvironment.t),
      ~state: Calc.t(EvaluatorState.t),
      ~editor: Calc.t(CodeSelectable.Model.t),
      ~ana: Calc.t(Typ.t),
      model
    ) =>
    option(
      (
        model,
        Calc.t(bool), // Hidden
        option((Calc.t(Exp.t), Calc.t(EvaluatorState.t))), // Next
        Calc.t(option(bool)) // Truth
      ),
    );

  let get_cursor_info: (~focus: focus, model) => Cursor.cursor(action);

  let handle_key_event:
    (~focus: focus, ~event: Key.t, model) => option(action);

  let view_justification:
    (
      ~globals: Globals.t,
      ~focus: option(focus),
      ~inject: action => Ui_effect.t(unit),
      ~take_focus: focus => Ui_effect.t(unit),
      ~hide_stepper: Ui_effect.t(unit),
      ~undo: option(Ui_effect.t(unit)),
      ~is_toplevel: bool,
      model
    ) =>
    WebUtil.Node.t;

  let view_content:
    (
      ~globals: Globals.t,
      ~focus: option(focus),
      ~inject: action => Ui_effect.t(unit),
      ~take_focus: focus => Ui_effect.t(unit),
      ~hide_stepper: Ui_effect.t(unit),
      ~undo: option(Ui_effect.t(unit)),
      ~is_toplevel: bool,
      model
    ) =>
    list(WebUtil.Node.t);
};

module type STEPPER = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type focus;

  let init: model;

  let update: (~settings: Settings.t, action, model) => Updated.t(model);

  let can_undo: action => bool;

  let calculate:
    (
      ~settings: Calc.t(CoreSettings.t),
      ~exp: Calc.t(Exp.t),
      ~ctx: Calc.t(Ctx.t),
      ~env: Calc.t(ClosureEnvironment.t),
      ~state: Calc.t(EvaluatorState.t),
      ~ana: Calc.t(Typ.t),
      model
    ) =>
    (model, Calc.t(Exp.t), Calc.t(option(bool)) /* Truth */);

  let get_cursor_info: (~focus: focus, model) => Cursor.cursor(action);

  let handle_key_event:
    (~focus: focus, ~event: Key.t, model) => option(action);

  let view:
    (
      ~globals: Globals.t,
      ~take_focus: focus => Ui_effect.t(unit),
      ~inject: action => Ui_effect.t(unit),
      ~hide_stepper: Ui_effect.t(unit),
      ~focus: option(focus),
      ~is_toplevel: bool,
      model
    ) =>
    list(WebUtil.Node.t);
};
