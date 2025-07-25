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
    | Jump(Action.jump_target)
    | Select(Action.select)
    | Unselect(option(Util.Direction.t))
    | Copy;

  let can_undo = (action: t) => {
    switch (action) {
    | Move(move) => Action.is_historic(Move(move))
    | Jump(target) => Action.is_historic(Jump(target))
    | Select(select) => Action.is_historic(Select(select))
    | Unselect(dir) => Action.is_historic(Unselect(dir))
    | Copy => false
    };
  };

  let update = (~globals, action: t, model: Model.t): Updated.t(Model.t) => {
    let action': CodeEditable.Update.t =
      switch (action) {
      | Move(move) => Perform(Move(move))
      | Jump(target) => Perform(Jump(target))
      | Select(select) => Perform(Select(select))
      | Unselect(dir) => Perform(Unselect(dir))
      | Copy => Perform(Copy)
      };
    CodeEditable.Update.update(~globals, action', model);
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
        Destruct(_) | Insert(_) | Put_down | Paste(_) | Reparse | Cut |
        Buffer(_) |
        Project(_) |
        Introduce,
      )
    | DebugConsole(_) => None;

  let calculate = CodeEditable.Update.calculate;
};

module Selection = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Editor.Focus.t;
  let get_cursor_info = (~inject) =>
    CodeEditable.Focus.get_cursor_info(
      ~inject=
        a =>
          switch (Update.convert_action(a)) {
          | Some(action) => inject(action)
          | None => Ui_effect.Ignore
          },
      ~read_only=true,
    );
};

module View = {
  type event =
    | MakeActive(Editor.Focus.t);

  let view = (~inject: Update.t => 'a, ~escape, ~take_focus, ~focus) =>
    Editor.View.view(
      ~mode=
        Editable({
          inject: a =>
            switch (Update.convert_action(Perform(a))) {
            | Some(action) => inject(action)
            | None => Ui_effect.Ignore
            },
          escape,
          take_focus,
          focus,
        }),
    );
};
