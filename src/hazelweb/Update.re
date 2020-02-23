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
let move_caret_here =
  JSUtil.dispatch_event([|
    Js.Unsafe.inject(Dom.Event.make("move_caret_here")),
  |]);

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
  | SelectionChange =>
    if (! state.setting_caret^) {
      let anchorNode = Dom_html.window##getSelection##.anchorNode;
      let anchorOffset = Dom_html.window##getSelection##.anchorOffset;
      let closest_elem = JSUtil.force_get_closest_elem(anchorNode);

      if (closest_elem |> has_cls("caret-position")) {
        if (closest_elem |> has_cls("has-caret")) {
          schedule_action(
            Action.EditAction(
              closest_elem |> has_cls("Before") ? MoveLeft : MoveRight,
            ),
          );
        } else {
          closest_elem |> move_caret_here;
        };
      } else if (closest_elem |> has_cls("caret-position-EmptyLine")) {
        if (closest_elem |> has_cls("has-caret")) {
          schedule_action(
            Action.EditAction(anchorOffset == 0 ? MoveLeft : MoveRight),
          );
        } else {
          closest_elem |> move_caret_here;
        };
      } else if (closest_elem |> has_cls("code-text")) {
        let evt =
          Js.Unsafe.(
            new_obj(
              global##_CustomEvent,
              [|
                inject(Js.string("move_caret_here")),
                obj([|("on_text", inject(anchorOffset))|]),
              |],
            )
          );
        closest_elem |> JSUtil.dispatch_event([|Js.Unsafe.inject(evt)|]);
      } else if (closest_elem
                 |> has_cls("Padding")
                 || closest_elem
                 |> has_cls("SpaceOp")) {
        let n = {
          let s = Js.Opt.get(anchorNode##.nodeValue, () => assert(false));
          s##.length;
        };
        let get_sibling_elem =
          anchorOffset * 2 <= n
            ? JSUtil.force_get_prev_sibling_elem
            : JSUtil.force_get_next_sibling_elem;
        closest_elem |> get_sibling_elem |> move_caret_here;
      } else if (closest_elem |> has_cls("trailing-whitespace")) {
        closest_elem |> JSUtil.force_get_prev_sibling_elem |> move_caret_here;
      } else if (closest_elem |> has_cls("leading-whitespace")) {
        let next_elem = {
          let next = closest_elem |> JSUtil.force_get_next_sibling_elem;
          next |> has_cls("Indent")
            ? next |> JSUtil.force_get_next_sibling_elem : next;
        };
        next_elem
        |> JSUtil.dispatch_event([|
             Js.Unsafe.inject(Dom.Event.make("move_caret_here")),
           |]);
      };
    };

    model;
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
  };
};
