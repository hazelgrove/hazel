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
    | SelectHoleInstance(HoleInstance.t)
    | InvalidVar(string)
    | FocusCell
    | BlurCell
    | Redo
    | Undo
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
  | SelectHoleInstance(_)
  | InvalidVar(_)
  | FocusCell
  | BlurCell
  | Undo
  | Redo
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
  | SelectHoleInstance(inst) => model |> Model.select_hole_instance(inst)
  | InvalidVar(_) => model
  | FocusCell => model |> Model.focus_cell
  | BlurCell => model |> Model.blur_cell
  | Undo =>
    let new_history = UndoHistory.undo(model.undo_history);
    let new_edit_state = ZList.prj_z(new_history);
    let new_model =
      model
      |> Model.put_program(
           Program.mk(~width=model.cell_width, new_edit_state),
         );
    {...new_model, undo_history: new_history};
  | Redo =>
    let new_history = UndoHistory.redo(model.undo_history);
    let new_edit_state = ZList.prj_z(new_history);
    let new_model =
      model
      |> Model.put_program(
           Program.mk(~width=model.cell_width, new_edit_state),
         );
    {...new_model, undo_history: new_history};
  | UpdateFontMetrics(metrics) => {...model, font_metrics: metrics}
  };
};
