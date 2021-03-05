module Js = Js_of_ocaml.Js;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;
module Sexp = Sexplib.Sexp;
open Sexplib.Std;

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
type timestamped_action = (timestamp, ModelAction.t);

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

let mk_timestamped_action = (a: ModelAction.t) => (
  get_current_timestamp(),
  a,
);

let log_action = (action: ModelAction.t, _: State.t): unit => {
  /* log interesting actions */
  switch (action) {
  | EditAction(_)
  | MoveAction(_)
  | LivelitAction(_)
  | ToggleLeftSidebar
  | ToggleRightSidebar
  | LoadExample(_)
  | LoadCardstack(_)
  | NextCard
  | PrevCard
  | SelectInstance(_)
  | UpdateSettings(_)
  | SelectCaseBranch(_)
  | InvalidVar(_)
  | FocusCell
  | BlurCell
  | FocusWindow
  | BlurWindow
  | Undo
  | Redo
  | ShiftHistory(_)
  | ShiftWhenScroll
  | ToggleHistoryGroup(_)
  | ToggleHiddenHistoryAll
  | TogglePreviewOnHover
  | UpdateFontMetrics(_)
  | UpdateIsMac(_)
  | ToggleTypingCtx
  | ToggleLivelitCtx =>
    Logger.append(
      Sexp.to_string(
        sexp_of_timestamped_action(mk_timestamped_action(action)),
      ),
    )
  };
};

let apply_action =
    (
      model: Model.t,
      action: ModelAction.t,
      state: State.t,
      ~schedule_action as _,
    )
    : Model.t => {
  let settings = model.settings;
  if (settings.performance.measure) {
    Printf.printf("\n== Update.apply_action times ==\n");
  };
  TimeUtil.measure_time(
    "Update.apply_action",
    settings.performance.measure && settings.performance.update_apply_action,
    () => {
      log_action(action, state);
      switch (action) {
      | EditAction(a) =>
        switch (model |> Model.perform_action(a)) {
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
        | exception Program.DoesNotElaborate =>
          JSUtil.log("[Program.DoesNotElaborate]");
          model;
        }
      | MoveAction(Key(move_key)) =>
        switch (Model.move_via_key(move_key, model)) {
        | exception Program.FailedAction =>
          JSUtil.log("[Program.FailedAction]");
          model;

        | None => model
        | Some(m) => m
        }
      | MoveAction(Click(opt_splice, row_col)) =>
        switch (model |> Model.move_via_click(opt_splice, row_col)) {
        | exception Program.FailedAction =>
          JSUtil.log("[Program.FailedAction]");
          model;
        | x => x
        }

      | LivelitAction(llu, serialized_action) =>
        let program = Model.get_program(model);
        let performed =
          try(
            model
            |> Model.perform_action(
                 ~livelit_move=true,
                 Program.move_to_node(Livelit, llu, program),
               )
            |> Model.perform_action(PerformLivelitAction(serialized_action))
          ) {
          | _ =>
            // NOTE(andrew): Modeling this error properly would be labor-intensive,
            // and will be unneeded after views move from js to hazel. Two options:
            // make the user provide a serialized type in the trigger fn, or check
            // the type of model dhexp returned from update; latter requires implementing
            // a typechecker for DHExp.
            print_endline(
              "ERROR: Livelit action exception: possibly action from js has wrong type",
            );
            model;
          };
        switch (Program.get_path(program)) {
        | None => Model.map_program(Program.blur, performed)
        | Some(path) =>
          Model.perform_action(~livelit_move=true, MoveTo(path), performed)
        };
      | ToggleLeftSidebar => Model.toggle_left_sidebar(model)
      | ToggleRightSidebar => Model.toggle_right_sidebar(model)
      | LoadExample(id) => Model.load_example(model, Examples.get(id))
      | LoadCardstack(idx) => Model.load_cardstack(model, idx)
      | NextCard => Model.next_card(model)
      | PrevCard => Model.prev_card(model)
      | SelectInstance(kind, inst) =>
        {...model, just_selected_instance: true}
        |> Model.select_instance((kind, inst))
      | SelectCaseBranch(path_to_case, branch_index) =>
        Model.select_case_branch(path_to_case, branch_index, model)
      | InvalidVar(_) => model
      | ToggleTypingCtx => {
          ...model,
          typing_ctx_open: !model.typing_ctx_open,
          // TODO rename
          just_selected_instance: true,
        }
      | ToggleLivelitCtx => {
          ...model,
          livelit_ctx_open: !model.livelit_ctx_open,
          // TODO rename
          just_selected_instance: true,
        }
      | FocusCell =>
        {...model, just_selected_instance: false}
        |> Model.map_program(Program.focus)
      | BlurCell =>
        let maybe_blur =
          JSUtil.window_has_focus() && !model.just_selected_instance
            ? Program.blur : (p => p);
        {...model, just_selected_instance: false}
        |> Model.map_program(maybe_blur);
      | FocusWindow =>
        {...model, just_selected_instance: false}
        |> Model.map_program(Program.focus_window)
      | BlurWindow =>
        {...model, just_selected_instance: false}
        |> Model.map_program(Program.blur_window)
      // | Undo =>
      //   let new_history =
      //     model.undo_history
      //     |> UndoHistory.shift_to_prev
      //     |> UndoHistory.update_disable_auto_scrolling(false);
      //   Model.load_undo_history(model, new_history, ~is_after_move=true);
      // | Redo =>
      //   let new_history =
      //     model.undo_history
      //     |> UndoHistory.shift_to_next
      //     |> UndoHistory.update_disable_auto_scrolling(false);
      //   Model.load_undo_history(model, new_history, ~is_after_move=true);
      // | ShiftHistory(shift_history_info) =>
      //   /* cshift to the certain entry */
      //   let new_history =
      //     model.undo_history
      //     |> UndoHistory.shift_history(
      //          shift_history_info.group_id,
      //          shift_history_info.elt_id,
      //          shift_history_info.call_by_mouseenter,
      //        );
      //   Model.load_
      | Undo
      | Redo
      | ShiftHistory(_)
      | ShiftWhenScroll => model
      | ToggleHistoryGroup(_) => model
      // | ToggleHistoryGroup(toggle_group_id) =>
      //   let (suc_groups, _, _) = model.undo_history.groups;
      //   let cur_group_id = List.length(suc_groups);
      //   /* shift to the toggle-target group and change its expanded state */
      //   switch (ZList.shift_to(toggle_group_id, model.undo_history.groups)) {
      //   | None =>
      //     failwith("Impossible match, because undo_history is non-empty")
      //   | Some(groups) =>
      //     let toggle_target_group = ZList.prj_z(groups);
      //     /* change expanded state of the toggle target group after toggling */
      //     let after_toggle =
      //       ZList.replace_z(
      //         {
      //           ...toggle_target_group,
      //           is_expanded: !toggle_target_group.is_expanded,
      //         },
      //         groups,
      //       );

      //     /*shift back to the current group*/
      //     switch (ZList.shift_to(cur_group_id, after_toggle)) {
      //     | None =>
      //       failwith("Impossible match, because undo_history is non-empty")
      //     | Some(new_groups) => {
      //         ...model,
      //         undo_history: {
      //           ...model.undo_history,
      //           groups: new_groups,
      //         },
      //       }
      //     };
      //   };
      | ToggleHiddenHistoryAll => model
      // |> Model.put_undo_history(
      //      UndoHistory.toggle_all_hidden_history(model.undo_history),
      //    )
      | TogglePreviewOnHover => model
      // {
      //   ...model,
      //   undo_history: {
      //     ...model.undo_history,
      //     preview_on_hover: !model.undo_history.preview_on_hover,
      //   },
      // }
      | UpdateFontMetrics(metrics) => {...model, font_metrics: metrics}
      | UpdateIsMac(is_mac) => {...model, is_mac}
      | UpdateSettings(u) => {
          ...model,
          settings: Settings.apply_update(u, model.settings),
        }
      };
    },
  );
};
