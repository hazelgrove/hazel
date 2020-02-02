module Js = Js_of_ocaml.Js;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;
module EditAction = Action;
module Sexp = Sexplib.Sexp;
open Sexplib.Std;
open ViewUtil;

module Action = {
  [@deriving sexp]
  type t =
    | EditAction(EditAction.t)
    | ToggleLeftSidebar
    | ToggleRightSidebar
    | LoadExample(Examples.id)
    | LoadCardStack(int)
    | NextCard
    | PrevCard
    | SetComputeResults(bool)
    | SetShowContentEditable(bool)
    | SetShowPresentation(bool)
    | SelectHoleInstance(MetaVar.t, MetaVarInst.t)
    | InvalidVar(string)
    | MoveToHole(MetaVar.t)
    | SelectionChange
    | FocusCell
    | BlurCell
    | FocusWindow
    | Redo
    | Undo
    | ShiftHistory(int, int)
    | ToggleHistoryGroup(int)
    | ToggleHiddenHistoryAll;
};

[@deriving sexp]
type timestamp = {
  year: int,
  month: int,
  day: int,
  hours: int,
  minutes: int,
  seconds: int,
  milliseconds: int,
};

[@deriving sexp]
type timestamped_action = (timestamp, Action.t);

let get_current_timestamp = (): timestamp => {
  let date = {
    %js
    new Js.date_now;
  };
  {
    year: date##getFullYear,
    month: date##getMonth,
    day: date##getDay,
    hours: date##getHours,
    minutes: date##getMinutes,
    seconds: date##getSeconds,
    milliseconds: date##getMilliseconds,
  };
};

let mk_timestamped_action = (a: Action.t) => (get_current_timestamp(), a);

let log_action = (action: Action.t, _: State.t): unit => {
  /* log interesting actions */
  switch (action) {
  | EditAction(_)
  | ToggleLeftSidebar
  | ToggleRightSidebar
  | LoadExample(_)
  | LoadCardStack(_)
  | NextCard
  | PrevCard
  | SetComputeResults(_)
  | SetShowContentEditable(_)
  | SetShowPresentation(_)
  | SelectHoleInstance(_, _)
  | InvalidVar(_)
  | FocusCell
  | BlurCell
  | FocusWindow
  | MoveToHole(_)
  | Undo
  | Redo
  | ShiftHistory(_, _)
  | ToggleHistoryGroup(_)
  | ToggleHiddenHistoryAll =>
    Logger.append(
      Sexp.to_string(
        sexp_of_timestamped_action(mk_timestamped_action(action)),
      ),
    )
  | SelectionChange => ()
  };
};

let apply_action =
    (model: Model.t, action: Action.t, state: State.t, ~schedule_action)
    : Model.t => {
  log_action(action, state);
  switch (action) {
  | EditAction(a) =>
    switch (Model.perform_edit_action(model, a)) {
    | new_model => new_model
    | exception Model.FailedAction =>
      JSUtil.log("[Model.FailedAction]");
      model;
    | exception Model.CursorEscaped =>
      JSUtil.log("[CursorEscaped]");
      model;
    | exception Model.MissingCursorInfo =>
      JSUtil.log("[MissingCursorInfo]");
      model;
    | exception Model.InvalidInput =>
      JSUtil.log("[Model.InvalidInput");
      model;
    | exception Model.DoesNotExpand =>
      JSUtil.log("[Model.DoesNotExpand]");
      model;
    }
  | ToggleLeftSidebar => Model.toggle_left_sidebar(model)
  | ToggleRightSidebar => Model.toggle_right_sidebar(model)
  | LoadExample(id) => Model.load_example(model, Examples.get(id))
  | LoadCardStack(idx) => Model.load_cardstack(model, idx)
  | NextCard =>
    state.changing_cards := true;
    Model.next_card(model);
  | PrevCard =>
    state.changing_cards := true;
    Model.prev_card(model);
  | SetComputeResults(compute_results) => {
      ...model,
      compute_results,
      result_state:
        Model.result_state_of_edit_state(
          Model.edit_state_of(model),
          compute_results,
        ),
    }
  | SetShowContentEditable(show_content_editable) => {
      ...model,
      show_content_editable,
    }
  | SetShowPresentation(show_presentation) => {...model, show_presentation}
  | SelectHoleInstance(u, i) => Model.select_hole_instance(model, (u, i))
  | InvalidVar(_) => model
  | MoveToHole(u) => Model.move_to_hole(model, u)
  | FocusCell => model |> Model.focus_cell
  | FocusWindow =>
    state.setting_caret := true;
    JSUtil.reset_caret();
    model;
  | BlurCell => JSUtil.window_has_focus() ? model |> Model.blur_cell : model
  | SelectionChange =>
    if (! state.setting_caret^) {
      let anchorNode = Dom_html.window##getSelection##.anchorNode;
      let anchorOffset = Dom_html.window##getSelection##.anchorOffset;
      let closest_elem = JSUtil.force_get_closest_elem(anchorNode);
      let id = closest_elem |> JSUtil.force_get_attr("id");
      switch (path_of_path_id(id), steps_of_text_id(id)) {
      | (None, None) => failwith(__LOC__ ++ ": unexpected caret position")
      | (Some((_, cursor) as path), _) =>
        if (path == Model.path(model)) {
          switch (cursor) {
          | OnText(_) => failwith(__LOC__ ++ ": unexpected cursor")
          | OnOp(Before)
          | OnDelim(_, Before) =>
            schedule_action(Action.EditAction(MoveLeft))
          | OnOp(After)
          | OnDelim(_, After) =>
            schedule_action(Action.EditAction(MoveRight))
          };
        } else {
          schedule_action(Action.EditAction(MoveTo(path)));
        }
      | (_, Some(steps)) =>
        if (closest_elem
            |> JSUtil.force_get_parent_elem
            |> JSUtil.elem_has_cls("EmptyLine")) {
          let (model_steps, _) = model |> Model.path;
          if (steps == model_steps) {
            schedule_action(
              Action.EditAction(anchorOffset == 0 ? MoveLeft : MoveRight),
            );
          } else {
            schedule_action(Action.EditAction(MoveTo((steps, OnText(0)))));
          };
        } else {
          schedule_action(
            Action.EditAction(MoveTo((steps, OnText(anchorOffset)))),
          );
        }
      };
    };

    model;
  | Undo => Model.undo(model)
  | Redo => Model.redo(model)
  /* click the history panel to shift to the certain history entry */
  | ShiftHistory(group_id, elt_id) =>
    /* shift to the group with group_id */
    switch (ZList.shift_to(group_id, model.undo_history)) {
    | None => failwith("Impossible match, because undo_history is non-empty")
    | Some(new_history) =>
      let cur_group = ZList.prj_z(new_history);
      /* shift to the element with elt_id */
      switch (ZList.shift_to(elt_id, cur_group.group_entries)) {
      | None => failwith("Impossible because group_entries is non-empty")
      | Some(new_group_entries) =>
        let new_cardstacks_state =
          ZList.prj_z(new_group_entries).cardstacks_state;
        let new_model =
          Model.update_cardstacks_state(model, new_cardstacks_state);
        {
          ...new_model,
          undo_history:
            ZList.replace_z(
              {...cur_group, group_entries: new_group_entries},
              new_history,
            ),
        };
      };
    }
  | ToggleHistoryGroup(toggle_group_id) =>
    let (suc_groups, _, _) = model.undo_history;
    let cur_group_id = List.length(suc_groups);
    /*shift to the toggle-target group and change its expanded state*/
    switch (ZList.shift_to(toggle_group_id, model.undo_history)) {
    | None => failwith("Impossible match, because undo_history is non-empty")
    | Some(history) =>
      let toggle_target_group = ZList.prj_z(history);
      /* change expanded state of the toggle target group after toggling */
      let after_toggle =
        ZList.replace_z(
          {
            ...toggle_target_group,
            is_expanded: !toggle_target_group.is_expanded,
          },
          history,
        );
      /*shift back to the current group*/
      switch (ZList.shift_to(cur_group_id, after_toggle)) {
      | None =>
        failwith("Impossible match, because undo_history is non-empty")
      | Some(new_history) => {...model, undo_history: new_history}
      };
    };
  | ToggleHiddenHistoryAll =>
    if (model.all_hidden_history_expand) {
      {
        ...model,
        all_hidden_history_expand: false,
        undo_history:
          UndoHistory.set_all_hidden_history(model.undo_history, false),
      };
    } else {
      {
        ...model,
        all_hidden_history_expand: true,
        undo_history:
          UndoHistory.set_all_hidden_history(model.undo_history, true),
      };
    }
  };
};
