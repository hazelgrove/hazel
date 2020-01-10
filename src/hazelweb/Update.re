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
    | Undo;
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
  | Redo =>
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
      | (None, None) =>
        JSUtil.log(id);
        failwith(__LOC__ ++ ": unexpected caret position");
      | (Some((_, cursor) as path), _) =>
        if (path == Model.path(model)) {
          switch (cursor) {
          | OnText(_) => failwith("unexpected OnText cursor")
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
        schedule_action(
          Action.EditAction(MoveTo((steps, OnText(anchorOffset)))),
        )
      };
    };

    model;
  | Undo =>
    let new_history = UndoHistory.undo(model.undo_history);
    let new_edit_state = ZList.prj_z(new_history);
    let new_model = model |> Model.update_edit_state(new_edit_state);
    {...new_model, undo_history: new_history};
  | Redo =>
    let new_history = UndoHistory.redo(model.undo_history);
    let new_edit_state = ZList.prj_z(new_history);
    let new_model = model |> Model.update_edit_state(new_edit_state);
    {...new_model, undo_history: new_history};
  };
};
