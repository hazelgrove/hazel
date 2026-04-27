open Haz3lcore;
open Util;

/* A CodeEditor that's been restricted to only performing selection with
   mouse/keyboard, no edits to the actual code. */
// This file follows conventions in [docs/ui-architecture.md]

module Model = CodeEditable.Model;

module Update = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Move(Action.move)
    | Select(Action.select)
    | Unselect(option(Util.Direction.t))
    | Copy;

  let can_undo = (action: t) => {
    switch (action) {
    | Move(move) => Action.is_historic(Move(move))
    | Select(select) => Action.is_historic(Select(select))
    | Unselect(dir) => Action.is_historic(Unselect(dir))
    | Copy => false
    };
  };

  let update = (~settings, action: t, model: Model.t): Updated.t(Model.t) => {
    let action': CodeEditable.Update.t =
      switch (action) {
      | Move(move) => Perform(Move(move))
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
    | Perform(Select(select)) => Some(Select(select))
    | Perform(Unselect(dir)) => Some(Unselect(dir))
    | Perform(Copy) => Some(Copy)

    // These actions are not allowed in a CodeSelectable
    | Perform(
        Destruct(_) | Insert(_) | Put_down | Paste(_) | Reparse | Cut |
        Buffer(_) |
        Project(_) |
        Structural(_) |
        Probe(_) |
        PrettyPrint |
        Dump |
        Introduce |
        ToggleLineComment,
      )
    | DebugConsole(_)
    | ContextMenu(_)
    | TAB => None;

  let calculate = CodeEditable.Update.calculate;
};

module Selection = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = CodeEditable.Selection.t;
  let get_cursor_info = (~inject as _, ~selection, model) =>
    CodeEditable.Selection.get_cursor_info(~selection, model)
    |> (
      ci =>
        Cursor.{
          ...ci,
          editor_read_only: true,
        }
    )
    |> Cursor.map_opt(Update.convert_action);
};

module View = {
  type event = CodeEditable.View.event;

  let props_of_edit_mode = (edit_mode: EditMode.t(Update.t, unit)) =>
    switch (edit_mode) {
    | ReadOnly => ((_ => Ui_effect.Ignore), false, (_ => Ui_effect.Ignore))
    | Editable({inject, escape, take_focus: _, focus}) => (
        (
          a =>
            switch (Update.convert_action(a)) {
            | Some(action) => inject(action)
            | None => Ui_effect.Ignore
            }
        ),
        focus != None,
        escape,
      )
    };

  let view =
      (
        ~edit_mode,
        ~globals,
        ~signal,
        ~overlays=?,
        ~lines=?,
        ~dynamics,
        ~expand_selection=?,
        model,
      ) => {
    let (inject, selected, escape) = props_of_edit_mode(edit_mode);
    CodeEditable.View.view(
      ~globals,
      ~signal,
      ~inject,
      ~selected,
      ~escape,
      ~overlays?,
      ~lines?,
      ~dynamics,
      ~expand_selection?,
      model,
    );
  };
};
