type t = {
  active: bool,
  selection_index: int,
  choice_display_limit: int,
  filter_editor: Program.typ,
};

let init_filter_editor = ty =>
  Program.Typ.mk(~width=80, ZTyp.place_before(ty));

let init = {
  active: false,
  selection_index: 0,
  choice_display_limit: 6,
  filter_editor: init_filter_editor(OpSeq.wrap(UHTyp.Hole)),
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
  | Reset_selection_index
  | Increment_selection_index
  | Decrement_selection_index;

let apply_update = (u: update, model: t) =>
  switch (u) {
  | Toggle => {...model, active: !model.active}
  | Turn_on => {...model, active: true}
  | Turn_off => {...model, active: false}
  | Reset_selection_index => {...model, selection_index: 0}
  | Increment_selection_index => {
      ...model,
      selection_index: model.selection_index + 1,
    }
  | Decrement_selection_index => {
      ...model,
      selection_index: model.selection_index - 1,
    }
  | Set_type_editor(uty) => put_filter_editor(model, init_filter_editor(uty))
  };
