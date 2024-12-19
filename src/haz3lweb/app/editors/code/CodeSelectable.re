open Haz3lcore;
open Util;

/* A CodeEditor that's been restricted to only performing selection with
   mouse/keyboard, no edits to the actual code. */
// This file follows conventions in [docs/ui-architecture.md]

module Model = EditorManager.Model;

module Update = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    // Ids refer to component IDs
    | Move(Id.t, Action.move)
    | Jump(Id.t, Action.jump_target)
    | Select(Id.t, Action.select)
    | Unselect(Id.t, option(Util.Direction.t))
    | Copy(Id.t);

  let update = (~settings, action: t, model: Model.t): Updated.t(Model.t) => {
    let action': EditorManager.Update.t =
      switch (action) {
      | Move(id, move) => Perform(id, Move(move))
      | Jump(id, target) => Perform(id, Jump(target))
      | Select(id, select) => Perform(id, Select(select))
      | Unselect(id, dir) => Perform(id, Unselect(dir))
      | Copy(id) => Perform(id, Copy)
      };
    EditorManager.Update.update(~settings, action', model);
  };

  let convert_action: EditorManager.Update.t => option(t) =
    fun
    // These actions are allowed in a CodeSelectable
    | Perform(id, Move(move)) => Some(Move(id, move))
    | Perform(id, Jump(target)) => Some(Jump(id, target))
    | Perform(id, Select(select)) => Some(Select(id, select))
    | Perform(id, Unselect(dir)) => Some(Unselect(id, dir))
    | Perform(id, Copy) => Some(Copy(id))

    // These actions are not allowed in a CodeSelectable
    | Perform(
        _,
        Destruct(_) | Insert(_) | RotateBackpack | MoveToBackpackTarget(_) |
        Pick_up |
        Put_down |
        Paste(_) |
        Reparse |
        Cut |
        Buffer(_) |
        Project(_),
      )
    | Undo(_)
    | Redo(_)
    | SetModel(_)
    | SetSyntax(_)
    | Manage(_)
    | TAB(_) => None;

  let calculate = EditorManager.Update.calculate;
};

module Selection = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = EditorManager.Focus.t;
  let get_cursor_info = (~selection, model) =>
    EditorManager.Focus.get_cursor_info(~selection, model)
    |> (ci => Cursor.{...ci, editor_read_only: true})
    |> Cursor.map_opt(Update.convert_action);
  let handle_key_event =
      (~selection, model: Model.t, key: Key.t): option(Update.t) =>
    EditorManager.Focus.handle_key_event(~selection, model, ~event=key)
    |> Option.bind(_, Update.convert_action);
};

module View = {
  type event = CodeEditable.View.event;

  let view = (~inject: Update.t => 'a) =>
    EditorManager.View.view(~inject=a =>
      switch (Update.convert_action(a)) {
      | Some(action) => inject(action)
      | None => Ui_effect.Ignore
      }
    );
};
