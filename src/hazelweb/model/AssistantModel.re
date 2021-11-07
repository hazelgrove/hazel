open OptUtil.Syntax;
open Sexplib.Std;

[@deriving sexp]
type display_mode =
  | Minimal
  | Normal;

type t = {
  active: bool,
  selection_index: int,
  hover_index: option(int),
  choice_display_limit: int,
  display_mode,
  retained_ci: option(CursorInfo.t),
  filter_editor: Editor.typ,
};

[@deriving sexp]
type update =
  | Turn_on
  | Turn_off
  | Toggle
  | Reset
  | Increment_selection_index
  | Decrement_selection_index
  | Set_hover_index(option(int))
  | Set_display_mode(display_mode)
  | Set_retained_ci(option(CursorInfo.t))
  | Set_type_editor(UHTyp.t);

let init = {
  active: false,
  display_mode: Normal,
  choice_display_limit: 6,
  selection_index: 0,
  hover_index: None,
  retained_ci: None,
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

let rec apply_update = (u: update, model: t, ci: CursorInfo.t) =>
  switch (u) {
  | Reset =>
    print_endline("Resetting incl. retained_ci");
    {
      ...init,
      retained_ci: Some(ci),
      active: model.active,
      display_mode: model.display_mode,
      choice_display_limit: model.choice_display_limit,
    };
  | Turn_off => {...apply_update(Reset, model, ci), active: false}
  | Turn_on => {...apply_update(Reset, model, ci), active: true}
  | Toggle => {...model, active: !model.active}
  | Increment_selection_index => {
      ...model,
      selection_index: model.selection_index + 1,
    }
  | Decrement_selection_index => {
      ...model,
      selection_index: model.selection_index - 1,
    }
  | Set_hover_index(n) => {...model, hover_index: n}
  | Set_display_mode(m) => {
      ...model,
      display_mode: m,
      choice_display_limit:
        switch (m) {
        | Minimal => 4
        | Normal => 6
        },
    }
  | Set_retained_ci(retained_ci) =>
    print_endline("Setting_retained_ci");
    {...model, retained_ci};
  | Set_type_editor(uty) =>
    put_filter_editor(model, Editor.mk_typ_editor(uty))
  };

let mk_suggestions =
    (
      {filter_editor, retained_ci, _}: t,
      ci: CursorInfo.t,
      ~u_gen: MetaVarGen.t,
    ) => {
  let ci =
    switch (retained_ci) {
    | None => ci
    | Some(ci) => ci
    };
  Suggestions.mk(~u_gen, ci, Editor.get_ty(filter_editor));
};

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

let action =
    (~u_gen: MetaVarGen.t=0, assistant_model: t, ci: CursorInfo.t)
    : option(Action.t) => {
  let+ selection = get_indicated_suggestion(assistant_model, ci, ~u_gen);
  Suggestion.action(selection);
};

let get_indicated_score = (model, ci) => {
  let+ selection = get_indicated_suggestion(model, ci, ~u_gen=0);
  Suggestion.score(selection);
};
