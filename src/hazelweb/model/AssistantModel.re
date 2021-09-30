open OptUtil.Syntax;
open Sexplib.Std;

type t = {
  active: bool,
  selection_index: int,
  hover_index: option(int),
  choice_display_limit: int,
  filter_editor: Editor.typ,
};

[@deriving sexp]
type update =
  | Toggle
  | Turn_on
  | Turn_off
  | Set_type_editor(UHTyp.t)
  | Reset
  | Increment_selection_index
  | Decrement_selection_index
  | Set_hover_index(option(int));

let init = {
  active: false,
  selection_index: 0,
  hover_index: None,
  choice_display_limit: 6,
  filter_editor: Editor.mk_typ_editor(OpSeq.wrap(UHTyp.Hole)),
};

let put_filter_editor = (assistant_model, filter_editor) => {
  ...assistant_model,
  filter_editor,
};

let update_filter_editor = (a: Action.t, new_editor, assistant_model: t): t => {
  let edit_state =
    new_editor
    |> Editor.get_edit_state
    |> Editor.EditState_Typ.perform_edit_action(a);
  put_filter_editor(assistant_model, {...new_editor, edit_state});
};

let set_hover_index = (hover_index: option(int), model: t): t => {
  ...model,
  hover_index,
};

let is_active_suggestion_index = (model: t, i: int) =>
  switch (model.hover_index) {
  | None => i == 0 // select first by default
  | Some(hover_index) => hover_index == i
  };

let is_hovering = (model: t) =>
  switch (model.hover_index) {
  | None => false
  | Some(_) => true
  };

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
  | Set_type_editor(uty) =>
    put_filter_editor(model, Editor.mk_typ_editor(uty))
  | Set_hover_index(n) => {...model, hover_index: n}
  };

let mk_suggestions =
    ({filter_editor, _}: t, ci: CursorInfo.t, ~u_gen: MetaVarGen.t) =>
  Suggestions.mk(~u_gen, ci, Editor.get_ty(filter_editor));

let wrap_index = (index, xs) => IntUtil.wrap(index, List.length(xs));

let get_display_suggestions =
    (
      ci: CursorInfo.t,
      ~u_gen: MetaVarGen.t=0,
      {choice_display_limit, selection_index, _} as model: t,
    )
    : Suggestions.t => {
  let suggestions = mk_suggestions(model, ci, ~u_gen);
  let wrapped_index = wrap_index(selection_index, suggestions);
  suggestions
  |> ListUtil.rotate_n(wrapped_index)
  |> ListUtil.trim(choice_display_limit);
};

let num_suggestions = (ci: CursorInfo.t, model: t) =>
  List.length(get_display_suggestions(ci, model));

let get_indicated_suggestion =
    (
      {hover_index, selection_index, _} as model: t,
      ci: CursorInfo.t,
      ~u_gen: MetaVarGen.t,
    ) => {
  let suggestions = mk_suggestions(model, ci, ~u_gen);
  let wrapped_index = wrap_index(selection_index, suggestions);
  let index =
    switch (hover_index) {
    | None => wrapped_index
    | Some(hover_index) =>
      IntUtil.wrap(wrapped_index + hover_index, List.length(suggestions))
    };
  List.nth_opt(suggestions, index);
};

let get_action =
    (~u_gen: MetaVarGen.t=0, assistant_model: t, ci: CursorInfo.t)
    : option(Action.t) => {
  let+ selection = get_indicated_suggestion(assistant_model, ci, ~u_gen);
  selection.action;
};
