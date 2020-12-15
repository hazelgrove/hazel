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
  | Undo
  | Redo
  | ShiftHistory(_)
  | ShiftWhenScroll
  | ToggleHistoryGroup(_)
  | ToggleHiddenHistoryAll
  | TogglePreviewOnHover
  | UpdateFontMetrics(_)
  | UpdateIsMac(_) =>
    Logger.append(
      Sexp.to_string(
        sexp_of_timestamped_action(mk_timestamped_action(action)),
      ),
    )
  };
};

let apply_action =
    (model: Model.t, action: ModelAction.t, state: State.t, ~schedule_action)
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
        let trigger = (llu, js_str) => {
          print_endline("RUN TRIGGER");
          print_endline(Js.to_string(js_str));
          let serialized_action =
            js_str
            |> Js.to_string
            |> Sexplib.Sexp.of_string
            |> SerializedAction.t_of_sexp;
          schedule_action(ModelAction.LivelitAction(llu, serialized_action));
        };
        Js.Unsafe.set(Dom_html.window, "trigger", Js.wrap_callback(trigger));

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
        };
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
        print_endline("Update.re: livelit action recieved:");
        print_endline(
          Sexplib.Sexp.to_string_hum(
            SerializedAction.sexp_of_t(serialized_action),
          ),
        );
        let program = Model.get_program(model);
        print_endline("Update.re: got model");
        let performed =
          model
          |> Model.perform_action(
               Program.move_to_node(Livelit, llu, program),
             )
          |> Model.perform_action(PerformLivelitAction(serialized_action));
        print_endline("Update.re: performed action");
        switch (Program.get_path(program)) {
        | None =>
          print_endline("Update.re: failed to get path");
          performed;
        | Some(path) =>
          print_endline("Update.re: got path");
          Model.perform_action(MoveTo(path), performed);
        };
      | ToggleLeftSidebar => Model.toggle_left_sidebar(model)
      | ToggleRightSidebar => Model.toggle_right_sidebar(model)
      | LoadExample(id) => Model.load_example(model, Examples.get(id))
      | LoadCardstack(idx) => Model.load_cardstack(model, idx)
      | NextCard => Model.next_card(model)
      | PrevCard => Model.prev_card(model)
      | SelectInstance(kind, inst) =>
        model |> Model.select_instance((kind, inst))
      | SelectCaseBranch(path_to_case, branch_index) =>
        Model.select_case_branch(path_to_case, branch_index, model)
      | InvalidVar(_) => model
      | FocusCell => model |> Model.map_program(Program.focus)
      | BlurCell => model |> Model.map_program(Program.blur)
      | Undo =>
        let new_history =
          model.undo_history
          |> UndoHistory.shift_to_prev
          |> UndoHistory.update_disable_auto_scrolling(false);
        Model.load_undo_history(model, new_history, ~is_after_move=true);
      | Redo =>
        let new_history =
          model.undo_history
          |> UndoHistory.shift_to_next
          |> UndoHistory.update_disable_auto_scrolling(false);
        Model.load_undo_history(model, new_history, ~is_after_move=true);
      | ShiftHistory(shift_history_info) =>
        /* cshift to the certain entry */
        let new_history =
          model.undo_history
          |> UndoHistory.shift_history(
               shift_history_info.group_id,
               shift_history_info.elt_id,
               shift_history_info.call_by_mouseenter,
             );
        Model.load_undo_history(model, new_history, ~is_after_move=false);
      | ShiftWhenScroll => model
      | ToggleHistoryGroup(toggle_group_id) =>
        let (suc_groups, _, _) = model.undo_history.groups;
        let cur_group_id = List.length(suc_groups);
        /* shift to the toggle-target group and change its expanded state */
        switch (ZList.shift_to(toggle_group_id, model.undo_history.groups)) {
        | None =>
          failwith("Impossible match, because undo_history is non-empty")
        | Some(groups) =>
          let toggle_target_group = ZList.prj_z(groups);
          /* change expanded state of the toggle target group after toggling */
          let after_toggle =
            ZList.replace_z(
              {
                ...toggle_target_group,
                is_expanded: !toggle_target_group.is_expanded,
              },
              groups,
            );

          /*shift back to the current group*/
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
        model
        |> Model.put_undo_history(
             UndoHistory.toggle_all_hidden_history(model.undo_history),
           )
      | TogglePreviewOnHover => {
          ...model,
          undo_history: {
            ...model.undo_history,
            preview_on_hover: !model.undo_history.preview_on_hover,
          },
        }
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
