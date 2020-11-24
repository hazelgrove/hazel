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
  | ToggleMemoizeDoc
  | SelectHoleInstance(_)
  | SelectCaseBranch(_)
  | InvalidVar(_)
  | FocusCell
  | ToggleMeasureTimes
  | ToggleMeasureModel_perform_edit_action
  | ToggleMeasureProgram_get_doc
  | ToggleMeasureLayoutOfDoc_layout_of_doc
  | ToggleMeasureUHCode_view
  | ToggleMeasureCell_view
  | ToggleMeasurePage_view
  | ToggleMeasureHazel_create
  | ToggleMeasureUpdate_apply_action
  | BlurCell
  | Undo
  | Redo
  | ShiftHistory(_)
  | ShiftWhenScroll
  | ToggleHistoryGroup(_)
  | ToggleHiddenHistoryAll
  | TogglePreviewOnHover
  | UpdateFontMetrics(_)
  | UpdateIsMac(_)
  | ToggleShowCursorInspector
  | ToggleCursorInspectorExpansion
  | ToggleTermNoviceMessageMode
  | ToggleTypeNoviceMessageMode
  | ToggleNoviceMode
  | SynthesizeHole(_)
  | ScrollFilling(_)
  | AcceptFilling =>
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
  if (model.measurements.measurements) {
    Printf.printf("\n== Update.apply_action times ==\n");
  };
  TimeUtil.measure_time(
    "Update.apply_action",
    model.measurements.measurements && model.measurements.update_apply_action,
    () => {
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
        | exception Program.DoesNotElaborate =>
          JSUtil.log("[Program.DoesNotElaborate]");
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
      | NextCard => Model.next_card(model)
      | PrevCard => Model.prev_card(model)
      | SynthesizeHole(u) => Model.synthesize(u, model)
      | ScrollFilling(i) =>
        switch (model.cursor_inspector.synthesizing) {
        | None => model
        | Some((u, _, fillings)) => {
            ...model,
            cursor_inspector: {
              ...model.cursor_inspector,
              synthesizing: Some((u, i, fillings)),
            },
          }
        }
      | AcceptFilling =>
        switch (model.cursor_inspector.synthesizing) {
        | None => model
        | Some((u, i, fillings)) =>
          schedule_action(
            ModelAction.EditAction(FillExpHole(u, List.nth(fillings, i))),
          );
          {
            ...model,
            cursor_inspector: {
              ...model.cursor_inspector,
              synthesizing: None,
            },
          };
        }
      //
      | ToggleComputeResults => {
          ...model,
          compute_results: {
            ...model.compute_results,
            compute_results: !model.compute_results.compute_results,
          },
        }
      | ToggleShowCaseClauses => {
          ...model,
          compute_results: {
            ...model.compute_results,
            show_case_clauses: !model.compute_results.show_case_clauses,
          },
        }
      | ToggleShowFnBodies => {
          ...model,
          compute_results: {
            ...model.compute_results,
            show_fn_bodies: !model.compute_results.show_fn_bodies,
          },
        }
      | ToggleShowCasts => {
          ...model,
          compute_results: {
            ...model.compute_results,
            show_casts: !model.compute_results.show_casts,
          },
        }
      | ToggleShowUnevaluatedExpansion => {
          ...model,
          compute_results: {
            ...model.compute_results,
            show_unevaluated_expansion:
              !model.compute_results.show_unevaluated_expansion,
          },
        }
      //
      | ToggleMeasureTimes => {
          ...model,
          measurements: {
            ...model.measurements,
            measurements: !model.measurements.measurements,
          },
        }
      | ToggleMeasureModel_perform_edit_action => {
          ...model,
          measurements: {
            ...model.measurements,
            model_perform_edit_action:
              !model.measurements.model_perform_edit_action,
          },
        }
      | ToggleMeasureProgram_get_doc => {
          ...model,
          measurements: {
            ...model.measurements,
            program_get_doc: !model.measurements.program_get_doc,
          },
        }
      | ToggleMeasureLayoutOfDoc_layout_of_doc => {
          ...model,
          measurements: {
            ...model.measurements,
            layoutOfDoc_layout_of_doc:
              !model.measurements.layoutOfDoc_layout_of_doc,
          },
        }
      | ToggleMeasureUHCode_view => {
          ...model,
          measurements: {
            ...model.measurements,
            uhcode_view: !model.measurements.uhcode_view,
          },
        }
      | ToggleMeasureCell_view => {
          ...model,
          measurements: {
            ...model.measurements,
            cell_view: !model.measurements.cell_view,
          },
        }
      | ToggleMeasurePage_view => {
          ...model,
          measurements: {
            ...model.measurements,
            page_view: !model.measurements.page_view,
          },
        }
      | ToggleMeasureHazel_create => {
          ...model,
          measurements: {
            ...model.measurements,
            hazel_create: !model.measurements.hazel_create,
          },
        }
      | ToggleMeasureUpdate_apply_action => {
          ...model,
          measurements: {
            ...model.measurements,
            update_apply_action: !model.measurements.update_apply_action,
          },
        }
      //
      | ToggleMemoizeDoc => {...model, memoize_doc: !model.memoize_doc}
      | SelectHoleInstance(inst) => model |> Model.select_hole_instance(inst)
      | SelectCaseBranch(path_to_case, branch_index) =>
        Model.select_case_branch(path_to_case, branch_index, model)
      | InvalidVar(_) => model
      | FocusCell => model |> Model.focus_cell
      | BlurCell => model |> Model.blur_cell
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
      | ToggleShowCursorInspector => {
          ...model,
          cursor_inspector: {
            ...model.cursor_inspector,
            visible: !model.cursor_inspector.visible,
          },
        }
      | ToggleCursorInspectorExpansion => {
          ...model,
          cursor_inspector: {
            ...model.cursor_inspector,
            show_expanded: !model.cursor_inspector.show_expanded,
          },
        }
      | ToggleTermNoviceMessageMode => {
          ...model,
          cursor_inspector: {
            ...model.cursor_inspector,
            term_novice_message_mode:
              !model.cursor_inspector.term_novice_message_mode,
          },
        }
      | ToggleTypeNoviceMessageMode => {
          ...model,
          cursor_inspector: {
            ...model.cursor_inspector,
            type_novice_message_mode:
              !model.cursor_inspector.type_novice_message_mode,
          },
        }
      | ToggleNoviceMode => Model.toggle_novice_mode(model)
      };
    },
  );
};
