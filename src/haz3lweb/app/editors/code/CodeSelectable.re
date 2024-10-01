open Haz3lcore;
open Util;

/* A CodeEditor that's been restricted to only performing selection with
   mouse/keyboard, no edits to the actual code. */

module Model = CodeEditable.Model;

module Update = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Move(Action.move)
    | Jump(Action.jump_target)
    | Select(Action.select)
    | Unselect(option(Util.Direction.t))
    | Copy;

  let update = (~settings, action: t, model: Model.t): Updated.t(Model.t) => {
    let action': CodeEditable.Update.t =
      switch (action) {
      | Move(move) => Perform(Move(move))
      | Jump(target) => Perform(Jump(target))
      | Select(select) => Perform(Select(select))
      | Unselect(dir) => Perform(Unselect(dir))
      | Copy => Perform(Copy)
      };
    CodeEditable.Update.update(~settings, action', model);
  };

  let convert_action: CodeEditable.Update.t => option(t) =
    fun
    // These actions are allowed in a CodeSelectable
    | Perform(Move(move)) => Some(Move(move))
    | Perform(Jump(target)) => Some(Jump(target))
    | Perform(Select(select)) => Some(Select(select))
    | Perform(Unselect(dir)) => Some(Unselect(dir))
    | Perform(Copy) => Some(Copy)

    // These actions are not allowed in a CodeSelectable
    | Perform(
        Destruct(_) | Insert(_) | RotateBackpack | MoveToBackpackTarget(_) |
        Pick_up |
        Put_down |
        Paste(_) |
        Reparse |
        Cut |
        Buffer(_) |
        Project(_),
      )
    | Undo
    | Redo
    | DebugConsole(_)
    | TAB => None;

  let calculate = CodeEditable.Update.calculate;
};

module Selection = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = CodeEditable.Selection.t;
  let get_cursor_info = (~selection, model) =>
    CodeEditable.Selection.get_cursor_info(~selection, model)
    |> Cursor.map_opt(Update.convert_action);
  let handle_key_event =
      (~selection, model: Model.t, key: Key.t): option(Update.t) =>
    CodeEditable.Selection.handle_key_event(~selection, model, key)
    |> Option.bind(_, Update.convert_action);
};

module View = {
  type event = CodeEditable.View.event;

  let view = (~inject: Update.t => 'a) =>
    CodeEditable.View.view(~inject=a =>
      switch (Update.convert_action(a)) {
      | Some(action) => inject(action)
      | None => Ui_effect.Ignore
      }
    );
};
