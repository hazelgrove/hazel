open OptUtil.Syntax;

type t = {
  active: bool,
  selection_index: int,
  choice_display_limit: int,
  filter_editor: Program.typ,
};

let mk_filter_editor = ty =>
  Program.Typ.mk(~width=80, ZTyp.place_before(ty));

let init = {
  active: false,
  selection_index: 0,
  choice_display_limit: 6,
  filter_editor: mk_filter_editor(OpSeq.wrap(UHTyp.Hole)),
};

let put_filter_editor = (assistant_model, filter_editor) => {
  ...assistant_model,
  filter_editor,
};

let update_filter_editor = (a: Action.t, new_editor, assistant_model: t): t => {
  let edit_state =
    new_editor
    |> Program.get_edit_state
    |> Program.EditState_Typ.perform_edit_action(a);
  put_filter_editor(assistant_model, {...new_editor, edit_state});
};

[@deriving sexp]
type update =
  | Toggle
  | Turn_on
  | Turn_off
  | Set_type_editor(UHTyp.t)
  | Reset
  | Increment_selection_index
  | Decrement_selection_index;

let apply_update = (u: update, model: t) =>
  switch (u) {
  | Turn_off => init
  | Turn_on => {...init, active: true}
  | Toggle => {...model, active: !model.active}
  | Reset => {...init, active: model.active}
  | Increment_selection_index => {
      ...model,
      selection_index: model.selection_index + 1,
    }
  | Decrement_selection_index => {
      ...model,
      selection_index: model.selection_index - 1,
    }
  | Set_type_editor(uty) => put_filter_editor(model, mk_filter_editor(uty))
  };

let normalize_index = (index: int, xs: list('a)): int =>
  IntUtil.wrap(index, List.length(xs));

let get_ty = (editor: Program.typ): HTyp.t =>
  editor
  |> Program.get_edit_state
  |> Program.EditState_Typ.get_uhstx
  |> UHTyp.expand;

let select_action =
    ({selection_index, filter_editor, _}: t, ci: CursorInfo.pro)
    : option(Action.t) => {
  let filter_ty = get_ty(filter_editor);
  let actions = Assistant.get_actions_of_ty(ci, filter_ty);
  //let actions = Assistant.get_actions(ci);
  let selection_index = normalize_index(selection_index, actions);
  let+ selection = List.nth_opt(actions, selection_index);
  selection.action;
};

let get_display_actions =
    (
      ci: CursorInfo.pro,
      {selection_index, choice_display_limit, filter_editor, _}: t,
    )
    : list(Assistant_Exp.assistant_action) => {
  let filter_ty = get_ty(filter_editor);
  //let actions = Assistant.get_actions(ci);
  let actions = Assistant.get_actions_of_ty(ci, filter_ty);
  let selection_index = normalize_index(selection_index, actions);
  actions
  |> ListUtil.rotate_n(selection_index)
  |> ListUtil.trim(choice_display_limit);
};
