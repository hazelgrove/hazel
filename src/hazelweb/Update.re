module Js = Js_of_ocaml.Js;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;
module EditAction = Action;
module Sexp = Sexplib.Sexp;
open Sexplib.Std;

[@deriving sexp]
type move_input =
  | Key(JSUtil.MoveKey.t)
  | Click((CursorMap.Row.t, CursorMap.Col.t));

module Action = {
  [@deriving sexp]
  type t =
    | EditAction(EditAction.t)
    | MoveAction(move_input)
    | ToggleLeftSidebar
    | ToggleRightSidebar
    | LoadExample(Examples.id)
    | LoadCardstack(int)
    | NextCard
    | PrevCard
    | ToggleComputeResults
    | ToggleShowCaseClauses
    | ToggleShowFnBodies
    | ToggleShowCasts
    | ToggleShowUnevaluatedExpansion
    | ToggleShowContenteditable
    | ToggleShowPresentation
    | SelectHoleInstance(HoleInstance.t)
    | InvalidVar(string)
    | FocusCell
    | BlurCell
    | Redo
    | Undo
    | ShiftHistory(int, int)
    | ToggleHistoryGroup(int)
    | ToggleHiddenHistoryAll
    | UpdateFontMetrics(FontMetrics.t);
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
  | MoveAction(_)
  | ToggleLeftSidebar
  | ToggleRightSidebar
  | LoadExample(_)
  | LoadCardstack(_)
  | NextCard
  | PrevCard
  | ToggleComputeResults
  | ToggleShowCaseClauses
  | ToggleShowFnBodies
  | ToggleShowCasts
  | ToggleShowUnevaluatedExpansion
  | ToggleShowContenteditable
  | ToggleShowPresentation
  | SelectHoleInstance(_)
  | InvalidVar(_)
  | FocusCell
  | BlurCell
  | Undo
  | Redo
  | ShiftHistory(_, _)
  | ToggleHistoryGroup(_)
  | ToggleHiddenHistoryAll
  | UpdateFontMetrics(_) =>
    Logger.append(
      Sexp.to_string(
        sexp_of_timestamped_action(mk_timestamped_action(action)),
      ),
    )
  };
};

let apply_action =
    (model: Model.t, action: Action.t, state: State.t, ~schedule_action as _)
    : Model.t => {
  log_action(action, state);
  switch (action) {
  | EditAction(a) =>
    switch (model |> Model.perform_edit_action(a)) {
    | new_model => new_model
    | exception Program.FailedAction =>
      JSUtil.log("[Program.FailedAction]");
      model;
    | exception Program.CursorEscaped =>
      JSUtil.log("[Program.CursorEscaped]");
      model;
    | exception Program.MissingCursorInfo =>
      JSUtil.log("[Program.MissingCursorInfo]");
      model;
    | exception Program.InvalidInput =>
      JSUtil.log("[Program.InvalidInput");
      model;
    | exception Program.DoesNotExpand =>
      JSUtil.log("[Program.DoesNotExpand]");
      model;
    }
  | MoveAction(Key(move_key)) =>
    switch (model |> Model.move_via_key(move_key)) {
    | new_model => new_model
    | exception Program.CursorEscaped =>
      JSUtil.log("[Program.CursorEscaped]");
      model;
    }
  | MoveAction(Click(row_col)) => model |> Model.move_via_click(row_col)
  | ToggleLeftSidebar => Model.toggle_left_sidebar(model)
  | ToggleRightSidebar => Model.toggle_right_sidebar(model)
  | LoadExample(id) => Model.load_example(model, Examples.get(id))
  | LoadCardstack(idx) => Model.load_cardstack(model, idx)
  | NextCard =>
    state.changing_cards := true;
    Model.next_card(model);
  | PrevCard =>
    state.changing_cards := true;
    Model.prev_card(model);
  | ToggleComputeResults => {
      ...model,
      compute_results: !model.compute_results,
    }
  | ToggleShowCaseClauses => {
      ...model,
      show_case_clauses: !model.show_case_clauses,
    }
  | ToggleShowFnBodies => {...model, show_fn_bodies: !model.show_fn_bodies}
  | ToggleShowCasts => {...model, show_casts: !model.show_casts}
  | ToggleShowUnevaluatedExpansion => {
      ...model,
      show_unevaluated_expansion: !model.show_unevaluated_expansion,
    }
  | ToggleShowContenteditable => {
      ...model,
      show_contenteditable: !model.show_contenteditable,
    }
  | ToggleShowPresentation => {
      ...model,
      show_presentation: !model.show_presentation,
    }
  | SelectHoleInstance(inst) => model |> Model.select_hole_instance(inst)
  | InvalidVar(_) => model
  | FocusCell => model |> Model.focus_cell
  | BlurCell => model |> Model.blur_cell
  | Undo =>
    let new_groups = {
      let cur_group = ZList.prj_z(model.undo_history.groups) /* shift to previous state in the same group */;
      switch (ZList.shift_next(cur_group.group_entries)) {
      | None =>
        /*if current group doesn't have previous state, shfit to previous group*/
        switch (ZList.shift_next(model.undo_history.groups)) {
        | None => model.undo_history.groups
        | Some(new_groups) =>
          let new_group = ZList.prj_z(new_groups);
          let new_entries = ZList.shift_begin(new_group.group_entries);
          let new_group': UndoHistory.undo_history_group = {
            group_entries: new_entries,
            is_expanded: true,
          } /* is_expanded=true because the selected group should be expanded*/;
          let groups = ZList.replace_z(new_group', new_groups);
          groups;
        }
      | Some(new_group_entries) =>
        let new_group: UndoHistory.undo_history_group = {
          group_entries: new_group_entries,
          is_expanded: true,
        };
        ZList.replace_z(
          new_group,
          model.undo_history.groups /* is_expanded=true because the selected group should be expanded*/,
        );
      };
    };
    let cur_group' = ZList.prj_z(new_groups);
    let new_cardstacks = ZList.prj_z(cur_group'.group_entries).cardstacks;
    let new_program = Cardstacks.get_program(new_cardstacks);
    let update_selected_instances = _ => {
      let si = UserSelectedInstances.init;
      switch (Program.cursor_on_exp_hole(new_program)) {
      | None => si
      | Some(u) => si |> UserSelectedInstances.insert_or_update((u, 0))
      };
    };
    let model' =
      model
      |> Model.put_cardstacks(new_cardstacks)
      |> Model.map_selected_instances(update_selected_instances);

    {
      ...model',
      undo_history: {
        ...model.undo_history,
        groups: new_groups,
      },
    };
  | Redo =>
    let new_groups = {
      let cur_group = ZList.prj_z(model.undo_history.groups) /* shift to previous state in the same group */;
      switch (ZList.shift_prev(cur_group.group_entries)) {
      | None =>
        /*if current group doesn't have previous state, shfit to previous group*/
        switch (ZList.shift_prev(model.undo_history.groups)) {
        | None => model.undo_history.groups
        | Some(new_groups) =>
          let new_group = ZList.prj_z(new_groups);
          let new_group': UndoHistory.undo_history_group = {
            group_entries: ZList.shift_end(new_group.group_entries) /*pointer may be in the wrong position after clicking an arbitrary entry in the history panel*/,
            is_expanded: true,
          } /* is_expanded=true because this group should be expanded when redo*/;
          ZList.replace_z(new_group', new_groups);
        }
      | Some(new_group_entries) =>
        let new_group: UndoHistory.undo_history_group = {
          group_entries: new_group_entries,
          is_expanded: true,
        };
        ZList.replace_z(
          new_group,
          model.undo_history.groups /* is_expanded=true because the selected group should be expanded*/,
        );
      };
    };
    let cur_group' = ZList.prj_z(new_groups);
    let new_cardstacks = ZList.prj_z(cur_group'.group_entries).cardstacks;

    let new_program = Cardstacks.get_program(new_cardstacks);
    let update_selected_instances = _ => {
      let si = UserSelectedInstances.init;
      switch (Program.cursor_on_exp_hole(new_program)) {
      | None => si
      | Some(u) => si |> UserSelectedInstances.insert_or_update((u, 0))
      };
    };
    let model' =
      model
      |> Model.put_cardstacks(new_cardstacks)
      |> Model.map_selected_instances(update_selected_instances);
    {
      ...model',
      undo_history: {
        ...model.undo_history,
        groups: new_groups,
      },
    };

  | ShiftHistory(group_id, elt_id) =>
    /* click the groups panel to shift to the certain groups entry */
    /* shift to the group with group_id */
    switch (ZList.shift_to(group_id, model.undo_history.groups)) {
    | None => failwith("Impossible match, because undo_history is non-empty")
    | Some(new_groups) =>
      let cur_group = ZList.prj_z(new_groups) /* shift to the element with elt_id */;
      switch (ZList.shift_to(elt_id, cur_group.group_entries)) {
      | None => failwith("Impossible because group_entries is non-empty")
      | Some(new_group_entries) =>
        let new_cardstacks = ZList.prj_z(new_group_entries).cardstacks;
        let new_model = model |> Model.put_cardstacks(new_cardstacks);
        let new_program = Cardstacks.get_program(new_cardstacks);
        let update_selected_instances = _ => {
          let si = UserSelectedInstances.init;
          switch (Program.cursor_on_exp_hole(new_program)) {
          | None => si
          | Some(u) => si |> UserSelectedInstances.insert_or_update((u, 0))
          };
        };
        let new_model' =
          new_model
          |> Model.put_cardstacks(new_cardstacks)
          |> Model.map_selected_instances(update_selected_instances);
        {
          ...new_model',
          undo_history: {
            ...new_model'.undo_history,
            groups:
              ZList.replace_z(
                {...cur_group, group_entries: new_group_entries},
                new_groups,
              ),
          },
        };
      };
    }
  | ToggleHistoryGroup(toggle_group_id) =>
    let (suc_groups, _, _) = model.undo_history.groups;
    let cur_group_id = List.length(suc_groups) /*shift to the toggle-target group and change its expanded state*/;
    switch (ZList.shift_to(toggle_group_id, model.undo_history.groups)) {
    | None => failwith("Impossible match, because undo_history is non-empty")
    | Some(groups) =>
      let toggle_target_group = ZList.prj_z(groups) /* change expanded state of the toggle target group after toggling */;
      let after_toggle =
        ZList.replace_z(
          {
            ...toggle_target_group,
            is_expanded: !toggle_target_group.is_expanded,
          },
          groups,
        ) /*shift back to the current group*/;
      switch (ZList.shift_to(cur_group_id, after_toggle)) {
      | None =>
        failwith("Impossible match, because undo_history is non-empty")
      | Some(new_groups) => {
          ...model,
          undo_history: {
            ...model.undo_history,
            groups: new_groups,
          },
        }
      };
    };
  | ToggleHiddenHistoryAll =>
    if (model.undo_history.all_hidden_history_expand) {
      {
        ...model,
        undo_history:
          UndoHistory.set_all_hidden_history(model.undo_history, false),
      };
    } else {
      {
        ...model,
        undo_history:
          UndoHistory.set_all_hidden_history(model.undo_history, true),
      };
    }
  | UpdateFontMetrics(metrics) => {...model, font_metrics: metrics}
  };
};
