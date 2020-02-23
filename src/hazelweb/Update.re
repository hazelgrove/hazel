module Js = Js_of_ocaml.Js;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;
module EditAction = Action;
module Sexp = Sexplib.Sexp;
open Sexplib.Std;

module Action = {
  [@deriving sexp]
  type t =
    | EditAction(EditAction.t)
    | ToggleLeftSidebar
    | ToggleRightSidebar
    | LoadExample(Examples.id)
    | LoadCardstack(int)
    | NextCard
    | PrevCard
    | SetComputeResults(bool)
    | SetShowContenteditable(bool)
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
  | LoadCardstack(_)
  | NextCard
  | PrevCard
  | SetComputeResults(_)
  | SetShowContenteditable(_)
  | SetShowPresentation(_)
  | SelectHoleInstance(_)
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

let has_cls = JSUtil.elem_has_cls;
let has_any_cls = (clss, elem) =>
  clss |> List.exists(cls => elem |> has_cls(cls));

let steps_of_caret_position =
    (elem: Js.t(Dom_html.element)): CursorPath.steps => {
  let steps = ref([]);
  let ancestor = ref(elem);
  // TODO use better root condition
  while (!(ancestor^ |> has_cls("code"))) {
    switch (ancestor^ |> JSUtil.get_attr("step")) {
    | None => ()
    | Some(step) =>
      let step = int_of_string(step);
      steps := [step, ...steps^];
    };
    ancestor := ancestor^ |> JSUtil.force_get_parent_elem;
  };
  steps^;
};

let rec schedule_move =
        (
          ~schedule_action: Action.t => unit,
          anchor_offset: int,
          anchor_parent: Js.t(Dom_html.element),
        )
        : unit => {
  let schedule_move' = schedule_move(~schedule_action);
  let schedule_move_prev' = schedule_move_prev(~schedule_action);
  let schedule_move_next' = schedule_move_next(~schedule_action);
  if (anchor_parent |> has_cls("caret-position")) {
    if (anchor_parent |> has_cls("has-caret")) {
      let schedule_move_to_neighbor =
        anchor_offset == 0 ? schedule_move_prev' : schedule_move_next';
      schedule_move_to_neighbor(anchor_parent);
    } else {
      let cursor: CursorPosition.t = {
        let side: Side.t =
          anchor_parent |> has_cls("Before") ? Before : After;
        let grandparent = anchor_parent |> JSUtil.force_get_parent_elem;
        if (grandparent |> has_cls("code-delim")) {
          let index =
            grandparent |> JSUtil.force_get_attr("index") |> int_of_string;
          OnDelim(index, side);
        } else {
          OnOp(side);
        };
      };
      let steps = steps_of_caret_position(anchor_parent);
      schedule_action(Action.EditAction(MoveTo((steps, cursor))));
    };
  } else if (anchor_parent |> has_cls("caret-position-EmptyLine")) {
    if (anchor_parent |> has_cls("has-caret")) {
      let schedule_move_to_neighbor =
        anchor_offset == 0 ? schedule_move_prev' : schedule_move_next';
      schedule_move_to_neighbor(anchor_parent);
    } else {
      let steps = steps_of_caret_position(anchor_parent);
      schedule_action(Action.EditAction(MoveTo((steps, OnText(0)))));
    };
  } else if (anchor_parent |> has_cls("code-text")) {
    let cursor = CursorPosition.OnText(anchor_offset);
    let steps = steps_of_caret_position(anchor_parent);
    schedule_action(Action.EditAction(MoveTo((steps, cursor))));
  } else if (anchor_parent
             |> has_any_cls(["Padding", "SpaceOp", "code-delim", "code-op"])) {
    let n = {
      let s = Js.Opt.get(anchor_parent##.textContent, () => assert(false));
      s##.length;
    };
    let get_sibling_elem =
      anchor_offset * 2 <= n
        ? JSUtil.force_get_prev_sibling_elem
        : JSUtil.force_get_next_sibling_elem;
    schedule_move'(0, anchor_parent |> get_sibling_elem);
  } else if (anchor_parent |> has_cls("trailing-whitespace")) {
    schedule_move'(0, anchor_parent |> JSUtil.force_get_prev_sibling_elem);
  } else if (anchor_parent |> has_cls("leading-whitespace")) {
    schedule_move'(0, anchor_parent |> JSUtil.force_get_next_sibling_elem);
  };
}
and schedule_move_prev = (~schedule_action, anchor_parent) => {
  let schedule_move' = schedule_move(~schedule_action);
  let schedule_move_prev' = schedule_move_prev(~schedule_action);
  // TODO use better root condition
  if (anchor_parent |> has_cls("code")) {
    ();
  } else if (anchor_parent |> has_cls("caret-position")) {
    schedule_move'(0, anchor_parent);
  } else if (anchor_parent |> has_cls("code-text")) {
    schedule_move'(
      anchor_parent |> JSUtil.inner_text |> String.length,
      anchor_parent,
    );
  } else {
    let prev =
      switch (anchor_parent |> JSUtil.get_prev_sibling_elem) {
      | None => anchor_parent |> JSUtil.force_get_parent_elem
      | Some(next) => next
      };
    schedule_move_prev'(prev);
  };
}
and schedule_move_next = (~schedule_action, anchor_parent) => {
  let schedule_move' = schedule_move(~schedule_action);
  let schedule_move_next' = schedule_move_next(~schedule_action);
  // TODO use better root condition
  if (anchor_parent |> has_cls("code")) {
    ();
  } else if (anchor_parent |> has_any_cls(["caret-position", "code-text"])) {
    schedule_move'(0, anchor_parent);
  } else {
    let next =
      switch (anchor_parent |> JSUtil.get_next_sibling_elem) {
      | None => anchor_parent |> JSUtil.force_get_parent_elem
      | Some(next) => next
      };
    schedule_move_next'(next);
  };
};

let apply_action =
    (model: Model.t, action: Action.t, state: State.t, ~schedule_action)
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
  | SetComputeResults(compute_results) => {...model, compute_results}
  | SetShowContenteditable(show_contenteditable) => {
      ...model,
      show_contenteditable,
    }
  | SetShowPresentation(show_presentation) => {...model, show_presentation}
  | SelectHoleInstance(u, i) => model |> Model.select_hole_instance((u, i))
  | InvalidVar(_) => model
  | MoveToHole(u) => model |> Model.move_to_hole(u)
  | FocusCell => model |> Model.focus_cell
  | FocusWindow =>
    state.setting_caret := true;
    JSUtil.reset_caret();
    model;
  | BlurCell => JSUtil.window_has_focus() ? model |> Model.blur_cell : model
  | Undo =>
    let new_history = UndoHistory.undo(model.undo_history);
    let new_edit_state = ZList.prj_z(new_history);
    let new_model = model |> Model.put_program(Program.mk(new_edit_state));
    {...new_model, undo_history: new_history};
  | Redo =>
    let new_history = UndoHistory.redo(model.undo_history);
    let new_edit_state = ZList.prj_z(new_history);
    let new_model = model |> Model.put_program(Program.mk(new_edit_state));
    {...new_model, undo_history: new_history};
  | SelectionChange =>
    if (! state.setting_caret^) {
      let anchor_offset = Dom_html.window##getSelection##.anchorOffset;
      let anchor_parent =
        Dom_html.window##getSelection##.anchorNode
        |> JSUtil.force_get_closest_elem;
      schedule_move(~schedule_action, anchor_offset, anchor_parent);
    };
    model;
  };
};
